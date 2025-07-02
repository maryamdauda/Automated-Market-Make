(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))
(define-constant ERR-INVALID-POSITION (err u102))
(define-constant ERR-CLAIM-TOO-EARLY (err u103))
(define-constant ERR-ALREADY-CLAIMED (err u104))

(define-data-var insurance-fund uint u0)
(define-data-var total-protected-liquidity uint u0)
(define-data-var protection-rate uint u8000)
(define-data-var minimum-protection-period uint u1008)
(define-data-var owner principal tx-sender)

(define-map protected-positions
    principal
    {
        liquidity-amount: uint,
        entry-price-x: uint,
        entry-price-y: uint,
        entry-block: uint,
        protection-claimed: bool,
        protection-fee-paid: uint
    }
)

(define-map insurance-contributions
    principal
    {
        total-contributed: uint,
        last-contribution: uint
    }
)

(define-data-var price-oracle-x uint u1000000)
(define-data-var price-oracle-y uint u1000000)

(define-read-only (get-insurance-fund-status)
    (ok {
        fund-balance: (var-get insurance-fund),
        total-protected: (var-get total-protected-liquidity),
        protection-rate: (var-get protection-rate),
        minimum-period: (var-get minimum-protection-period)
    })
)

(define-read-only (get-user-position (user principal))
    (ok (map-get? protected-positions user))
)

(define-public (contribute-to-insurance-fund (amount uint))
    (let
        (
            (current-contribution (default-to {total-contributed: u0, last-contribution: u0} 
                                 (map-get? insurance-contributions tx-sender)))
        )
        (begin
            (var-set insurance-fund (+ (var-get insurance-fund) amount))
            (map-set insurance-contributions tx-sender
                {
                    total-contributed: (+ (get total-contributed current-contribution) amount),
                    last-contribution: stacks-block-height
                }
            )
            (ok true)
        )
    )
)

(define-public (register-protected-position (liquidity-amount uint))
    (let
        (
            (protection-fee (/ (* liquidity-amount (var-get protection-rate)) u100000))
            (current-price-x (var-get price-oracle-x))
            (current-price-y (var-get price-oracle-y))
        )
        (begin
            (asserts! (> liquidity-amount u0) ERR-INVALID-POSITION)
            (asserts! (is-none (map-get? protected-positions tx-sender)) ERR-INVALID-POSITION)
            (var-set insurance-fund (+ (var-get insurance-fund) protection-fee))
            (var-set total-protected-liquidity (+ (var-get total-protected-liquidity) liquidity-amount))
            (map-set protected-positions tx-sender
                {
                    liquidity-amount: liquidity-amount,
                    entry-price-x: current-price-x,
                    entry-price-y: current-price-y,
                    entry-block: stacks-block-height,
                    protection-claimed: false,
                    protection-fee-paid: protection-fee
                }
            )
            (ok true)
        )
    )
)

(define-private (calculate-impermanent-loss (entry-price-x uint) (entry-price-y uint) (current-price-x uint) (current-price-y uint) (liquidity-amount uint))
    (let
        (
            (price-ratio-entry (/ (* entry-price-x u1000000) entry-price-y))
            (price-ratio-current (/ (* current-price-x u1000000) current-price-y))
            (price-change-factor (if (> price-ratio-current price-ratio-entry)
                                   (/ (* price-ratio-current u1000000) price-ratio-entry)
                                   (/ (* price-ratio-entry u1000000) price-ratio-current)))
            (sqrt-factor
                (let
                    (
                        (sqrt-n price-change-factor)
                        (sqrt-x price-change-factor)
                        (sqrt-y (+ (/ price-change-factor u2) u1))
                    )
                    (if (<= sqrt-n u1)
                        sqrt-n
                        (let
                            (
                                (sqrt-iter
                                    (let ((n sqrt-n) (y sqrt-y) (x sqrt-x))
                                        (let ((new-y (/ (+ y (/ n y)) u2)))
                                            (if (< new-y y)
                                                (let ((n2 n) (y2 new-y) (x2 y))
                                                    (let ((new-y2 (/ (+ y2 (/ n2 y2)) u2)))
                                                        (if (< new-y2 y2)
                                                            new-y2
                                                            y2
                                                        )
                                                    )
                                                )
                                                y
                                            )
                                        )
                                    )
                                )
                            )
                            sqrt-iter
                        )
                    )
                )
            )
            (hodl-value liquidity-amount)
            (lp-value (/ (* u2000000 liquidity-amount) (+ u1000000 sqrt-factor)))
            (impermanent-loss (if (> hodl-value lp-value) (- hodl-value lp-value) u0))
        )
        impermanent-loss
    )
)
(define-read-only (calculate-user-impermanent-loss (user principal))
    (let
        (
            (position (unwrap! (map-get? protected-positions user) ERR-INVALID-POSITION))
            (current-price-x (var-get price-oracle-x))
            (current-price-y (var-get price-oracle-y))
            (il-amount (calculate-impermanent-loss 
                       (get entry-price-x position)
                       (get entry-price-y position)
                       current-price-x
                       current-price-y
                       (get liquidity-amount position)))
        )
        (ok il-amount)
    )
)

(define-public (claim-impermanent-loss-protection)
    (let
        (
            (position (unwrap! (map-get? protected-positions tx-sender) ERR-INVALID-POSITION))
            (current-block stacks-block-height)
            (protection-period-passed (> (- current-block (get entry-block position)) (var-get minimum-protection-period)))
            (il-amount (unwrap! (calculate-user-impermanent-loss tx-sender) ERR-INVALID-POSITION))
            (max-compensation (/ (* (get liquidity-amount position) u20) u100))
            (compensation (if (> il-amount max-compensation) max-compensation il-amount))
        )
        (begin
            (asserts! protection-period-passed ERR-CLAIM-TOO-EARLY)
            (asserts! (not (get protection-claimed position)) ERR-ALREADY-CLAIMED)
            (asserts! (>= (var-get insurance-fund) compensation) ERR-INSUFFICIENT-FUNDS)
            (asserts! (> compensation u0) ERR-INVALID-POSITION)
            (var-set insurance-fund (- (var-get insurance-fund) compensation))
            (var-set total-protected-liquidity (- (var-get total-protected-liquidity) (get liquidity-amount position)))
            (map-set protected-positions tx-sender
                (merge position {protection-claimed: true})
            )
            (ok compensation)
        )
    )
)

(define-public (update-price-oracle (new-price-x uint) (new-price-y uint))
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (var-set price-oracle-x new-price-x)
        (var-set price-oracle-y new-price-y)
        (ok true)
    )
)

(define-public (adjust-protection-parameters (new-rate uint) (new-period uint))
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-rate u10000) ERR-INVALID-POSITION)
        (var-set protection-rate new-rate)
        (var-set minimum-protection-period new-period)
        (ok true)
    )
)

(define-public (emergency-fund-withdrawal (amount uint))
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= amount (var-get insurance-fund)) ERR-INSUFFICIENT-FUNDS)
        (var-set insurance-fund (- (var-get insurance-fund) amount))
        (ok amount)
    )
)

(define-read-only (get-protection-cost (liquidity-amount uint))
    (ok (/ (* liquidity-amount (var-get protection-rate)) u100000))
)

(define-public (remove-protection)
    (let
        (
            (position (unwrap! (map-get? protected-positions tx-sender) ERR-INVALID-POSITION))
        )
        (begin
            (asserts! (not (get protection-claimed position)) ERR-ALREADY-CLAIMED)
            (var-set total-protected-liquidity (- (var-get total-protected-liquidity) (get liquidity-amount position)))
            (map-delete protected-positions tx-sender)
            (ok true)
        )
    )
)