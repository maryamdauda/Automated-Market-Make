;; Dynamic Fee AMM - MVP
;; Implements a basic AMM with fee adjustment based on volatility

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-POOL-EMPTY (err u102))

;; Data vars
(define-data-var token-x-balance uint u0)
(define-data-var token-y-balance uint u0)
(define-data-var last-price uint u0)
(define-data-var base-fee uint u30) ;; 0.3% base fee
(define-data-var current-fee uint u30)
(define-data-var owner principal tx-sender)

;; Get current pool balances
(define-read-only (get-balances)
    (ok {
        token-x: (var-get token-x-balance),
        token-y: (var-get token-y-balance),
        current-fee: (var-get current-fee)
    })
)

;; Add liquidity
(define-public (add-liquidity (amount-x uint) (amount-y uint))
    (let
        (
            (current-x (var-get token-x-balance))
            (current-y (var-get token-y-balance))
        )
        (begin
            (var-set token-x-balance (+ current-x amount-x))
            (var-set token-y-balance (+ current-y amount-y))
            (ok true)
        )
    )
)

;; Calculate dynamic fee based on price change
(define-private (calculate-dynamic-fee (new-price uint))
    (let
        (
            (last-saved-price (var-get last-price))
            (price-change (if (> new-price last-saved-price)
                (- new-price last-saved-price)
                (- last-saved-price new-price)))
            (base (var-get base-fee))
        )
        (if (> price-change u100)
            (+ base u10) ;; Increase fee during high volatility
            base
        )
    )
)

;; Swap tokens
(define-public (swap-x-for-y  (amount-x uint) (min-y-out uint) )
    (let
        (
            (x-balance (var-get token-x-balance))
            (y-balance (var-get token-y-balance))
            (new-x-balance (+ x-balance amount-x))
            (constant (* x-balance y-balance))
            (new-y-balance (/ constant new-x-balance))
            (y-out (- y-balance new-y-balance))
            (new-price (/ (* amount-x u1000000) y-out))
            (dynamic-fee (calculate-dynamic-fee new-price))
        )
        (begin
            (asserts! (>= y-out min-y-out) (err u104))
            (asserts! (< (calculate-price-impact amount-x x-balance) MAX-PRICE-IMPACT) (err u106))
            (asserts! (not (var-get is-paused)) (err u103)) ;; Add at start of function
            (var-set current-fee dynamic-fee)
            (var-set last-price new-price)
            (var-set token-x-balance new-x-balance)
            (var-set token-y-balance new-y-balance)
            (ok y-out)
        )
    )
)


;; Add at the top with other data vars
(define-data-var is-paused bool false)

;; Add new function
(define-public (toggle-pause)
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set is-paused (not (var-get is-paused))))
    )
)



(define-data-var external-price uint u0)
(define-data-var price-timestamp uint u0)

(define-read-only (get-price-info)
    (ok {
        amm-price: (var-get last-price),
        oracle-price: (var-get external-price),
        timestamp: (var-get price-timestamp)
    })
)


;; Add new data vars
(define-map lp-shares principal uint)
(define-data-var total-shares uint u0)

(define-public (claim-rewards (amount uint))
    (let (
        (user-shares (default-to u0 (map-get? lp-shares tx-sender)))
        (reward-amount (/ (* amount user-shares) (var-get total-shares)))
    )
        (begin
            (asserts! (> user-shares u0) ERR-INSUFFICIENT-BALANCE)
            (ok reward-amount)
        )
    )
)



;; Add new data vars
(define-map token-balances (string-ascii 32) uint)
(define-map token-pairs
    {token-a: (string-ascii 32), token-b: (string-ascii 32)}
    {active: bool, fee: uint}
)

(define-public (add-token-pair 
    (token-a (string-ascii 32)) 
    (token-b (string-ascii 32)) 
    (initial-fee uint)
)
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set token-pairs
            {token-a: token-a, token-b: token-b}
            {active: true, fee: initial-fee}
        ))
    )
)



(define-public (remove-token-pair 
    (token-a (string-ascii 32)) 
    (token-b (string-ascii 32))
)
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set token-pairs
            {token-a: token-a, token-b: token-b}
            {active: false, fee: u0}
        ))
    )
)




(define-public (swap-tokens 
    (token-a (string-ascii 32)) 
    (token-b (string-ascii 32)) 
    (amount-a uint) 
    (min-amount-b uint)
)
    (let (
        (pair (unwrap! (map-get? token-pairs {token-a: token-a, token-b: token-b}) ERR-POOL-EMPTY))
    )
        (begin
            (asserts! (get active pair) ERR-POOL-EMPTY)
            (ok true)
        )
    )
)


(define-constant ERR-NO-FUNDS (err u105))

(define-public (emergency-withdraw)
    (let (
        (user-shares (default-to u0 (map-get? lp-shares tx-sender)))
        (x-amount (/ (* (var-get token-x-balance) user-shares) (var-get total-shares)))
        (y-amount (/ (* (var-get token-y-balance) user-shares) (var-get total-shares)))
    )
        (begin
            (asserts! (> user-shares u0) ERR-NO-FUNDS)
            (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
            (var-set token-x-balance (- (var-get token-x-balance) x-amount))
            (var-set token-y-balance (- (var-get token-y-balance) y-amount))
            (ok {withdrawn-x: x-amount, withdrawn-y: y-amount})
        )
    )
)



(define-constant MAX-PRICE-IMPACT u50) ;; 5% max impact

(define-private (calculate-price-impact (amount-in uint) (balance-in uint))
    (let (
        (impact (/ (* amount-in u1000) balance-in))
    )
        impact
    )
)


(define-map daily-volumes principal uint)
(define-constant VOLUME-TIER-1 u1000000) ;; 1M volume
(define-constant VOLUME-TIER-2 u5000000) ;; 5M volume

(define-private (get-volume-based-fee (user principal))
    (let (
        (user-volume (default-to u0 (map-get? daily-volumes user)))
    )
        (if (>= user-volume VOLUME-TIER-2)
            u20 ;; 0.2% fee
            (if (>= user-volume VOLUME-TIER-1)
                u25 ;; 0.25% fee
                u30 ;; default 0.3% fee
            )
        )
    )
)



(define-data-var rewards-pool uint u0)
(define-constant REWARD-RATE u5) ;; 0.05% of fees

(define-public (distribute-rewards)
    (let (
        (reward-amount (/ (* (var-get rewards-pool) REWARD-RATE) u100))
    )
        (begin
            (var-set rewards-pool (- (var-get rewards-pool) reward-amount))
            (ok reward-amount)
        )
    )
)



(define-map price-observations uint {price: uint, timestamp: uint})
(define-data-var current-observation-index uint u0)

(define-public (record-price)
    (begin
        (map-set price-observations (var-get current-observation-index)
            {
                price: (var-get last-price),
                timestamp: stacks-block-height
            }
        )
        (var-set current-observation-index (+ (var-get current-observation-index) u1))
        (ok true)
    )
)



(define-constant MAX-SLIPPAGE u30) ;; 3% max slippage

(define-private (check-slippage (expected uint) (actual uint))
    (let (
        (slippage (/ (* (- expected actual) u1000) expected))
    )
        (<= slippage MAX-SLIPPAGE)
    )
)



(define-map supported-tokens (string-ascii 32) {active: bool, decimals: uint})

(define-public (add-supported-token (token-symbol (string-ascii 32)) (decimals uint))
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set supported-tokens token-symbol {active: true, decimals: decimals}))
    )
)



(define-data-var mining-rate uint u100) ;; Tokens per block
(define-map staking-positions principal {amount: uint, start-block: uint})

(define-public (stake-liquidity (amount uint))
    (begin
        (asserts! (> amount u0) ERR-INSUFFICIENT-BALANCE)
        (ok (map-set staking-positions tx-sender 
            {amount: amount, start-block: stacks-block-height}))
    )
)


(define-constant FLASH-LOAN-FEE u10) ;; 0.1% fee

(define-public (flash-loan (amount uint))
    (let (
        (fee (/ (* amount FLASH-LOAN-FEE) u10000))
    )
        (begin
            (asserts! (<= amount (var-get token-x-balance)) ERR-INSUFFICIENT-BALANCE)
            (ok {loan-amount: amount, fee: fee})
        )
    )
)


(define-map referral-rewards principal uint)
(define-constant REFERRAL-REWARD-RATE u10) ;; 0.1% of swap amount

(define-public (register-referral (referrer principal) (amount uint))
    (let (
        (reward (/ (* amount REFERRAL-REWARD-RATE) u10000))
    )
        (begin
            (map-set referral-rewards referrer 
                (+ (default-to u0 (map-get? referral-rewards referrer)) reward))
            (ok reward)
        )
    )
)
