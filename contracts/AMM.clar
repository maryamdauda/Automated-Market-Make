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


;; Add at top with other data vars
(define-data-var twap-cumulative uint u0)
(define-data-var last-update-time uint u0)

(define-public (update-twap)
    (let (
        (current-time stacks-block-height)
        (time-elapsed (- current-time (var-get last-update-time)))
        (current-price (var-get last-price))
    )
        (begin
            (var-set twap-cumulative (+ (var-get twap-cumulative) (* current-price time-elapsed)))
            (var-set last-update-time current-time)
            (ok (/ (var-get twap-cumulative) current-time))
        )
    )
)


(define-map lp-tiers principal 
    {
        tier: uint,
        multiplier: uint,
        min-stake: uint
    }
)

(define-public (set-lp-tier (stake-amount uint))
    (let (
        (tier-info (if (>= stake-amount u1000000)
            {tier: u3, multiplier: u150, min-stake: u1000000}
            (if (>= stake-amount u500000)
                {tier: u2, multiplier: u125, min-stake: u500000}
                {tier: u1, multiplier: u100, min-stake: u0}
            )
        ))
    )
        (ok (map-set lp-tiers tx-sender tier-info))
    )
)


(define-map limit-orders 
    {id: uint}
    {
        owner: principal,
        token-in: (string-ascii 32),
        token-out: (string-ascii 32),
        amount-in: uint,
        min-price: uint,
        expiry: uint
    }
)
(define-data-var order-counter uint u0)

(define-public (place-limit-order 
    (token-in (string-ascii 32))
    (token-out (string-ascii 32))
    (amount-in uint)
    (min-price uint)
    (expiry uint)
)
    (let (
        (order-id (var-get order-counter))
    )
        (begin
            (var-set order-counter (+ order-id u1))
            (ok (map-set limit-orders
                {id: order-id}
                {
                    owner: tx-sender,
                    token-in: token-in,
                    token-out: token-out,
                    amount-in: amount-in,
                    min-price: min-price,
                    expiry: expiry
                }
            ))
        )
    )
)



(define-map trading-cooldowns principal uint)
(define-constant COOLDOWN-PERIOD u10) ;; blocks

(define-private (check-trading-cooldown)
    (let (
        (last-trade (default-to u0 (map-get? trading-cooldowns tx-sender)))
        (current-block stacks-block-height)
    )
        (if (> (- current-block last-trade) COOLDOWN-PERIOD)
            (begin
                (map-set trading-cooldowns tx-sender current-block)
                true
            )
            false
        )
    )
)



(define-map pool-weights 
    (string-ascii 32)
    {weight: uint, last-update: uint}
)

(define-public (adjust-pool-weight 
    (token (string-ascii 32))
    (new-weight uint)
)
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set pool-weights token 
            {
                weight: new-weight,
                last-update: stacks-block-height
            }
        ))
    )
)


(define-data-var mining-enabled bool true)
(define-data-var reward-per-block uint u100)
(define-map miner-rewards principal uint)

(define-public (claim-mining-rewards)
    (let (
        (user-stake (default-to u0 (map-get? lp-shares tx-sender)))
        (blocks-staked (- stacks-block-height (var-get last-update-time)))
        (reward (* blocks-staked (var-get reward-per-block)))
    )
        (begin
            (asserts! (var-get mining-enabled) ERR-NOT-AUTHORIZED)
            (asserts! (> user-stake u0) ERR-INSUFFICIENT-BALANCE)
            (map-set miner-rewards tx-sender (+ (default-to u0 (map-get? miner-rewards tx-sender)) reward))
            (ok reward)
        )
    )
)


(define-map fee-sharing-points principal uint)
(define-data-var total-fee-points uint u0)

(define-public (register-fee-sharing)
    (let (
        (user-liquidity (default-to u0 (map-get? lp-shares tx-sender)))
        (points (/ (* user-liquidity u100) (var-get total-shares)))
    )
        (begin
            (map-set fee-sharing-points tx-sender points)
            (var-set total-fee-points (+ (var-get total-fee-points) points))
            (ok points)
        )
    )
)


(define-map circuit-breakers
    (string-ascii 32)
    {
        threshold: uint,
        triggered: bool,
        cool-down: uint
    }
)

(define-public (set-circuit-breaker
    (breaker-id (string-ascii 32))
    (threshold uint)
    (cool-down uint)
)
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set circuit-breakers breaker-id
            {
                threshold: threshold,
                triggered: false,
                cool-down: cool-down
            }
        ))
    )
)


(define-map governance-signers principal bool)
(define-data-var required-signatures uint u3)
(define-data-var proposal-counter uint u0)

(define-map governance-proposals 
    {id: uint} 
    {
        proposer: principal,
        description: (string-ascii 100),
        signatures: uint,
        executed: bool,
        expiry: uint
    }
)

(define-map proposal-votes 
    {proposal-id: uint, signer: principal} 
    bool
)

(define-public (add-governance-signer (signer principal))
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set governance-signers signer true))
    )
)

(define-public (create-proposal (description (string-ascii 100)) (expiry uint))
    (let
        (
            (proposal-id (var-get proposal-counter))
        )
        (begin
            (asserts! (default-to false (map-get? governance-signers tx-sender)) ERR-NOT-AUTHORIZED)
            (var-set proposal-counter (+ proposal-id u1))
            (ok (map-set governance-proposals 
                {id: proposal-id}
                {
                    proposer: tx-sender,
                    description: description,
                    signatures: u1,
                    executed: false,
                    expiry: expiry
                }
            ))
        )
    )
)

(define-public (sign-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? governance-proposals {id: proposal-id}) ERR-NOT-AUTHORIZED))
            (current-signatures (get signatures proposal))
        )
        (begin
            (asserts! (default-to false (map-get? governance-signers tx-sender)) ERR-NOT-AUTHORIZED)
            (asserts! (not (default-to false (map-get? proposal-votes {proposal-id: proposal-id, signer: tx-sender}))) ERR-NOT-AUTHORIZED)
            (map-set proposal-votes {proposal-id: proposal-id, signer: tx-sender} true)
            (map-set governance-proposals 
                {id: proposal-id}
                (merge proposal {signatures: (+ current-signatures u1)})
            )
            (ok true)
        )
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? governance-proposals {id: proposal-id}) ERR-NOT-AUTHORIZED))
        )
        (begin
            (asserts! (>= (get signatures proposal) (var-get required-signatures)) ERR-NOT-AUTHORIZED)
            (asserts! (not (get executed proposal)) ERR-NOT-AUTHORIZED)
            (asserts! (< stacks-block-height (get expiry proposal)) ERR-NOT-AUTHORIZED)
            (map-set governance-proposals 
                {id: proposal-id}
                (merge proposal {executed: true})
            )
            (ok true)
        )
    )
)



(define-data-var mining-start-block uint u0)
(define-data-var mining-end-block uint u0)
(define-data-var mining-rewards-per-block uint u100)
(define-data-var total-mining-rewards uint u0)
(define-map user-mining-info principal {last-claim-block: uint, pending-rewards: uint})

(define-public (initialize-mining-program (start-block uint) (end-block uint) (rewards-per-block uint) (total-rewards uint))
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (var-set mining-start-block start-block)
        (var-set mining-end-block end-block)
        (var-set mining-rewards-per-block rewards-per-block)
        (var-set total-mining-rewards total-rewards)
        (ok true)
    )
)

(define-private (calculate-user-rewards (user principal))
    (let
        (
            (user-info (default-to {last-claim-block: (var-get mining-start-block), pending-rewards: u0} 
                        (map-get? user-mining-info user)))
            (user-shares (default-to u0 (map-get? lp-shares user)))
            (current-block (if (> stacks-block-height (var-get mining-end-block)) 
                              (var-get mining-end-block) 
                              stacks-block-height))
            (blocks-since-last-claim (- current-block (get last-claim-block user-info)))
            (total-pool-shares (var-get total-shares))
            (user-share-percentage (if (> total-pool-shares u0)
                                      (/ (* user-shares u10000) total-pool-shares)
                                      u0))
            (new-rewards (/ (* blocks-since-last-claim (var-get mining-rewards-per-block) user-share-percentage) u10000))
            (total-pending (+ (get pending-rewards user-info) new-rewards))
        )
        {
            rewards: total-pending,
            last-block: current-block
        }
    )
)

(define-read-only (get-pending-mining-rewards (user principal))
    (ok (get rewards (calculate-user-rewards user)))
)

(define-data-var fee-adjustment-period uint u144) ;; ~1 day in blocks
(define-data-var last-fee-adjustment uint u0)
(define-data-var volatility-threshold uint u500) ;; 5% threshold
(define-data-var max-fee uint u100) ;; 1% max fee
(define-data-var min-fee uint u10) ;; 0.1% min fee
(define-map historical-prices uint uint)

(define-public (record-historical-price)
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
        (map-set historical-prices stacks-block-height (var-get last-price))
        (ok true)
    )
)

(define-private (calculate-volatility)
    (let
        (
            (current-block stacks-block-height)
            (lookback-period (var-get fee-adjustment-period))
            (lookback-block (- current-block lookback-period))
            (old-price (default-to u0 (map-get? historical-prices lookback-block)))
            (current-price (var-get last-price))
            (price-diff (if (and (> old-price u0) (> current-price u0))
                          (if (> current-price old-price)
                            (/ (* (- current-price old-price) u10000) old-price)
                            (/ (* (- old-price current-price) u10000) old-price))
                          u0))
        )
        price-diff
    )
)

(define-public (adjust-fee-based-on-volatility)
    (let
        (
            (current-block stacks-block-height)
            (volatility (calculate-volatility))
            (new-fee (if (> volatility (var-get volatility-threshold))
                        ;; High volatility - increase fee
                        (if (> (+ (var-get current-fee) u10) (var-get max-fee))
                            (var-get max-fee)
                            (+ (var-get current-fee) u10))
                        ;; Low volatility - decrease fee
                        (if (< (- (var-get current-fee) u5) (var-get min-fee))
                            (var-get min-fee)
                            (- (var-get current-fee) u5))))
        )
        (begin
            (asserts! (> (- current-block (var-get last-fee-adjustment)) (var-get fee-adjustment-period)) ERR-NOT-AUTHORIZED)
            (var-set current-fee new-fee)
            (var-set last-fee-adjustment current-block)
            (ok new-fee)
        )
    )
)


(define-data-var twap-period uint u144) ;; ~1 day in blocks
(define-map price-accumulator uint {cumulative-price: uint, timestamp: uint})
(define-data-var last-observation-index uint u0)
(define-data-var observation-count uint u24) ;; Store 24 observations

(define-public (update-price-accumulator)
    (let
        (
            (current-block stacks-block-height)
            (current-price (var-get last-price))
            (last-observation (default-to {cumulative-price: u0, timestamp: current-block} 
                              (map-get? price-accumulator (var-get last-observation-index))))
            (time-elapsed (- current-block (get timestamp last-observation)))
            (new-cumulative-price (+ (get cumulative-price last-observation) (* current-price time-elapsed)))
            (new-index (mod (+ (var-get last-observation-index) u1) (var-get observation-count)))
        )
        (begin
            (map-set price-accumulator new-index 
                {
                    cumulative-price: new-cumulative-price,
                    timestamp: current-block
                }
            )
            (var-set last-observation-index new-index)
            (ok new-cumulative-price)
        )
    )
)

(define-read-only (get-twap)
    (let
        (
            (current-block stacks-block-height)
            (current-index (var-get last-observation-index))
            (lookback-index (mod (+ current-index u1) (var-get observation-count)))
            (current-observation (default-to {cumulative-price: u0, timestamp: current-block} 
                                (map-get? price-accumulator current-index)))
            (lookback-observation (default-to {cumulative-price: u0, timestamp: u0} 
                                 (map-get? price-accumulator lookback-index)))
            (price-diff (- (get cumulative-price current-observation) (get cumulative-price lookback-observation)))
            (time-diff (- (get timestamp current-observation) (get timestamp lookback-observation)))
        )
        (if (> time-diff u0)
            (ok (/ price-diff time-diff))
            (ok (var-get last-price))
        )
    )
)







(define-map staking-boost-info 
    principal 
    {
        boost-multiplier: uint,
        lock-duration: uint,
        lock-start: uint,
        staked-amount: uint
    }
)

(define-constant BOOST-LEVELS 
    {
        base: u100,
        bronze: u125,
        silver: u150,
        gold: u200
    }
)

(define-constant LOCK-PERIODS
    {
        month-1: u4320,
        month-3: u12960,
        month-6: u25920
    }
)

(define-public (stake-with-boost (amount uint) (lock-duration uint))
    (let
        (
            (multiplier (if (>= lock-duration (get month-6 LOCK-PERIODS))
                           (get gold BOOST-LEVELS)
                           (if (>= lock-duration (get month-3 LOCK-PERIODS))
                               (get silver BOOST-LEVELS)
                               (get bronze BOOST-LEVELS))))
        )
        (begin
            (asserts! (>= amount u1000000) ERR-INSUFFICIENT-BALANCE)
            (map-set staking-boost-info tx-sender
                {
                    boost-multiplier: multiplier,
                    lock-duration: lock-duration,
                    lock-start: stacks-block-height,
                    staked-amount: amount
                }
            )
            (ok multiplier)
        )
    )
)