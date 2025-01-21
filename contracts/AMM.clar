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
(define-public (swap-x-for-y (amount-x uint))
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
            (var-set current-fee dynamic-fee)
            (var-set last-price new-price)
            (var-set token-x-balance new-x-balance)
            (var-set token-y-balance new-y-balance)
            (ok y-out)
        )
    )
)
