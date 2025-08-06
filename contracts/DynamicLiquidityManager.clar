;; Dynamic Liquidity Pool Manager
;; Automatically optimizes liquidity distribution across multiple trading pairs

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-POOL-NOT-FOUND (err u102))
(define-constant ERR-INVALID-PARAMETERS (err u103))
(define-constant ERR-REBALANCE-COOLDOWN (err u104))
(define-constant ERR-INSUFFICIENT-LIQUIDITY (err u105))

;; Contract owner and configuration
(define-data-var owner principal tx-sender)
(define-data-var pool-counter uint u0)
(define-data-var total-managed-liquidity uint u0)
(define-data-var rebalance-cooldown uint u144) ;; ~1 day in blocks
(define-data-var min-liquidity-threshold uint u1000000) ;; Minimum liquidity required

;; Pool performance tracking
(define-map managed-pools
    {pool-id: uint}
    {
        token-pair: (string-ascii 32),
        current-liquidity: uint,
        target-allocation: uint, ;; Percentage (0-10000 = 0-100%)
        current-allocation: uint,
        volume-24h: uint,
        fee-revenue-24h: uint,
        volatility-score: uint,
        efficiency-score: uint,
        last-rebalance: uint,
        active: bool
    }
)

;; Liquidity provider positions in managed pools
(define-map lp-positions
    {pool-id: uint, provider: principal}
    {
        deposited-amount: uint,
        share-percentage: uint,
        entry-block: uint,
        accumulated-fees: uint,
        performance-multiplier: uint
    }
)

;; Historical performance metrics for optimization algorithms
(define-map performance-history
    {pool-id: uint, period: uint}
    {
        volume: uint,
        fees: uint,
        price-volatility: uint,
        liquidity-utilization: uint,
        profit-score: uint
    }
)

;; Rebalancing strategies and thresholds
(define-map rebalance-strategies
    (string-ascii 32)
    {
        enabled: bool,
        weight: uint,
        threshold: uint,
        cooldown-override: uint
    }
)

;; Dynamic allocation weights based on market conditions
(define-map allocation-weights
    (string-ascii 32) ;; Strategy name
    {
        volume-weight: uint,
        volatility-weight: uint,
        fee-weight: uint,
        efficiency-weight: uint
    }
)

;; Automated market-making configuration
(define-data-var auto-rebalance-enabled bool true)
(define-data-var performance-window uint u1008) ;; 7 days in blocks
(define-data-var max-single-pool-allocation uint u4000) ;; 40% max allocation
(define-data-var min-single-pool-allocation uint u500) ;; 5% min allocation

;; Initialize the dynamic liquidity manager
(define-public (initialize-manager)
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        ;; Set up default rebalancing strategies
        (map-set rebalance-strategies "volume-weighted"
            {enabled: true, weight: u3000, threshold: u1000, cooldown-override: u0})
        (map-set rebalance-strategies "volatility-adjusted"
            {enabled: true, weight: u2000, threshold: u500, cooldown-override: u0})
        (map-set rebalance-strategies "fee-optimized"
            {enabled: true, weight: u3000, threshold: u750, cooldown-override: u0})
        (map-set rebalance-strategies "efficiency-based"
            {enabled: true, weight: u2000, threshold: u800, cooldown-override: u0})
        ;; Set default allocation weights
        (map-set allocation-weights "balanced"
            {volume-weight: u2500, volatility-weight: u2500, fee-weight: u2500, efficiency-weight: u2500})
        (ok true)
    )
)

;; Register a new managed pool
(define-public (register-managed-pool
    (token-pair (string-ascii 32))
    (initial-allocation uint)
    (target-allocation uint)
)
    (let
        (
            (pool-id (var-get pool-counter))
        )
        (begin
            (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
            (asserts! (<= target-allocation u10000) ERR-INVALID-PARAMETERS)
            (asserts! (>= target-allocation (var-get min-single-pool-allocation)) ERR-INVALID-PARAMETERS)
            (asserts! (<= target-allocation (var-get max-single-pool-allocation)) ERR-INVALID-PARAMETERS)
            (var-set pool-counter (+ pool-id u1))
            (map-set managed-pools
                {pool-id: pool-id}
                {
                    token-pair: token-pair,
                    current-liquidity: initial-allocation,
                    target-allocation: target-allocation,
                    current-allocation: u0,
                    volume-24h: u0,
                    fee-revenue-24h: u0,
                    volatility-score: u1000,
                    efficiency-score: u1000,
                    last-rebalance: stacks-block-height,
                    active: true
                }
            )
            (var-set total-managed-liquidity (+ (var-get total-managed-liquidity) initial-allocation))
            (ok pool-id)
        )
    )
)

;; Deposit liquidity into a managed pool
(define-public (deposit-liquidity (pool-id uint) (amount uint))
    (let
        (
            (pool (unwrap! (map-get? managed-pools {pool-id: pool-id}) ERR-POOL-NOT-FOUND))
            (existing-position (default-to 
                {deposited-amount: u0, share-percentage: u0, entry-block: u0, accumulated-fees: u0, performance-multiplier: u100}
                (map-get? lp-positions {pool-id: pool-id, provider: tx-sender})))
            (new-liquidity (+ (get current-liquidity pool) amount))
            (total-liquidity (var-get total-managed-liquidity))
            (new-share (if (> total-liquidity u0)
                          (/ (* amount u10000) total-liquidity)
                          u10000))
        )
        (begin
            (asserts! (get active pool) ERR-POOL-NOT-FOUND)
            (asserts! (> amount u0) ERR-INSUFFICIENT-BALANCE)
            (map-set lp-positions
                {pool-id: pool-id, provider: tx-sender}
                {
                    deposited-amount: (+ (get deposited-amount existing-position) amount),
                    share-percentage: (+ (get share-percentage existing-position) new-share),
                    entry-block: stacks-block-height,
                    accumulated-fees: (get accumulated-fees existing-position),
                    performance-multiplier: u100
                }
            )
            (map-set managed-pools
                {pool-id: pool-id}
                (merge pool {current-liquidity: new-liquidity})
            )
            (var-set total-managed-liquidity (+ total-liquidity amount))
            (ok true)
        )
    )
)

;; Calculate optimal allocation based on performance metrics
(define-private (calculate-optimal-allocation (pool-id uint))
    (match (map-get? managed-pools {pool-id: pool-id})
        pool
        (let
            (
                (volume-score (/ (get volume-24h pool) u1000))
                (fee-score (/ (get fee-revenue-24h pool) u100))
                (volatility-penalty (/ (get volatility-score pool) u100))
                (efficiency-bonus (/ (get efficiency-score pool) u100))
                (weights (default-to 
                    {volume-weight: u2500, volatility-weight: u2500, fee-weight: u2500, efficiency-weight: u2500}
                    (map-get? allocation-weights "balanced")))
                (composite-score (+ 
                    (/ (* volume-score (get volume-weight weights)) u10000)
                    (/ (* fee-score (get fee-weight weights)) u10000)
                    (/ (* efficiency-bonus (get efficiency-weight weights)) u10000)
                    (- u1000 (/ (* volatility-penalty (get volatility-weight weights)) u10000))))
                (optimal-allocation (if (> composite-score u10000)
                                       (var-get max-single-pool-allocation)
                                       (if (< composite-score (var-get min-single-pool-allocation))
                                           (var-get min-single-pool-allocation)
                                           composite-score)))
            )
            optimal-allocation
        )
        u0  ;; Return 0 if pool not found
    )
)

;; Update performance metrics for a pool
(define-public (update-pool-metrics
    (pool-id uint)
    (volume-24h uint)
    (fee-revenue-24h uint)
    (volatility-score uint)
)
    (let
        (
            (pool (unwrap! (map-get? managed-pools {pool-id: pool-id}) ERR-POOL-NOT-FOUND))
            (efficiency-score (if (> volume-24h u0)
                                 (/ (* fee-revenue-24h u10000) volume-24h)
                                 u1000))
            (current-period (/ stacks-block-height (var-get performance-window)))
        )
        (begin
            (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
            (map-set managed-pools
                {pool-id: pool-id}
                (merge pool {
                    volume-24h: volume-24h,
                    fee-revenue-24h: fee-revenue-24h,
                    volatility-score: volatility-score,
                    efficiency-score: efficiency-score
                })
            )
            ;; Store historical data
            (map-set performance-history
                {pool-id: pool-id, period: current-period}
                {
                    volume: volume-24h,
                    fees: fee-revenue-24h,
                    price-volatility: volatility-score,
                    liquidity-utilization: (/ (* (get current-liquidity pool) u10000) (var-get total-managed-liquidity)),
                    profit-score: efficiency-score
                }
            )
            (ok true)
        )
    )
)

;; Execute automatic rebalancing across all managed pools
(define-public (execute-rebalancing)
    (let
        (
            (current-block stacks-block-height)
            (total-liquidity (var-get total-managed-liquidity))
        )
        (begin
            (asserts! (var-get auto-rebalance-enabled) ERR-NOT-AUTHORIZED)
            (asserts! (> total-liquidity (var-get min-liquidity-threshold)) ERR-INSUFFICIENT-LIQUIDITY)
            ;; This would iterate through pools in a real implementation
            ;; For this MVP, we'll demonstrate the rebalancing logic for pool 0
            (try! (rebalance-single-pool u0))
            (ok true)
        )
    )
)

;; Rebalance a single pool based on optimization algorithms
(define-private (rebalance-single-pool (pool-id uint))
    (let
        (
            (pool (unwrap! (map-get? managed-pools {pool-id: pool-id}) ERR-POOL-NOT-FOUND))
            (current-block stacks-block-height)
            (time-since-rebalance (- current-block (get last-rebalance pool)))
            (optimal-allocation (calculate-optimal-allocation pool-id))
            (current-allocation (get current-allocation pool))
            (allocation-diff (if (> optimal-allocation current-allocation)
                               (- optimal-allocation current-allocation)
                               (- current-allocation optimal-allocation)))
        )
        (begin
            (asserts! (> time-since-rebalance (var-get rebalance-cooldown)) ERR-REBALANCE-COOLDOWN)
            (asserts! (get active pool) ERR-POOL-NOT-FOUND)
            (asserts! (> allocation-diff u100) ERR-INVALID-PARAMETERS) ;; Only rebalance if diff > 1%
            (map-set managed-pools
                {pool-id: pool-id}
                (merge pool {
                    target-allocation: optimal-allocation,
                    current-allocation: optimal-allocation,
                    last-rebalance: current-block
                })
            )
            (ok true)
        )
    )
)

;; Claim accumulated fees from liquidity provision
(define-public (claim-liquidity-fees (pool-id uint))
    (let
        (
            (position (unwrap! (map-get? lp-positions {pool-id: pool-id, provider: tx-sender}) ERR-POOL-NOT-FOUND))
            (pool (unwrap! (map-get? managed-pools {pool-id: pool-id}) ERR-POOL-NOT-FOUND))
            (fee-share (/ (* (get fee-revenue-24h pool) (get share-percentage position)) u10000))
            (performance-bonus (/ (* fee-share (get performance-multiplier position)) u100))
            (total-claimable (+ fee-share performance-bonus))
        )
        (begin
            (asserts! (> total-claimable u0) ERR-INSUFFICIENT-BALANCE)
            (map-set lp-positions
                {pool-id: pool-id, provider: tx-sender}
                (merge position {accumulated-fees: u0})
            )
            (ok total-claimable)
        )
    )
)

;; Withdraw liquidity from a managed pool
(define-public (withdraw-liquidity (pool-id uint) (amount uint))
    (let
        (
            (pool (unwrap! (map-get? managed-pools {pool-id: pool-id}) ERR-POOL-NOT-FOUND))
            (position (unwrap! (map-get? lp-positions {pool-id: pool-id, provider: tx-sender}) ERR-POOL-NOT-FOUND))
            (max-withdrawal (get deposited-amount position))
            (new-liquidity (- (get current-liquidity pool) amount))
        )
        (begin
            (asserts! (<= amount max-withdrawal) ERR-INSUFFICIENT-BALANCE)
            (asserts! (> amount u0) ERR-INVALID-PARAMETERS)
            (map-set lp-positions
                {pool-id: pool-id, provider: tx-sender}
                (merge position {deposited-amount: (- max-withdrawal amount)})
            )
            (map-set managed-pools
                {pool-id: pool-id}
                (merge pool {current-liquidity: new-liquidity})
            )
            (var-set total-managed-liquidity (- (var-get total-managed-liquidity) amount))
            (ok amount)
        )
    )
)

;; Read-only functions for monitoring and analytics
(define-read-only (get-pool-info (pool-id uint))
    (ok (map-get? managed-pools {pool-id: pool-id}))
)

(define-read-only (get-lp-position (pool-id uint) (provider principal))
    (ok (map-get? lp-positions {pool-id: pool-id, provider: provider}))
)

(define-read-only (get-manager-status)
    (ok {
        total-managed-liquidity: (var-get total-managed-liquidity),
        active-pools: (var-get pool-counter),
        auto-rebalance-enabled: (var-get auto-rebalance-enabled),
        rebalance-cooldown: (var-get rebalance-cooldown)
    })
)

(define-read-only (get-performance-history (pool-id uint) (period uint))
    (ok (map-get? performance-history {pool-id: pool-id, period: period}))
)

;; Administrative functions
(define-public (toggle-auto-rebalance)
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (var-set auto-rebalance-enabled (not (var-get auto-rebalance-enabled)))
        (ok (var-get auto-rebalance-enabled))
    )
)

(define-public (update-rebalance-parameters (new-cooldown uint) (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (var-set rebalance-cooldown new-cooldown)
        (var-set min-liquidity-threshold new-threshold)
        (ok true)
    )
)

(define-public (deactivate-pool (pool-id uint))
    (let
        (
            (pool (unwrap! (map-get? managed-pools {pool-id: pool-id}) ERR-POOL-NOT-FOUND))
        )
        (begin
            (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
            (map-set managed-pools
                {pool-id: pool-id}
                (merge pool {active: false})
            )
            (ok true)
        )
    )
)

