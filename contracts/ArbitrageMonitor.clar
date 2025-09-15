;; Cross-Chain Arbitrage Monitor
;; Tracks price differences across chains and markets to detect arbitrage opportunities

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PRICE (err u101))
(define-constant ERR-ORACLE-NOT-FOUND (err u102))
(define-constant ERR-ARBITRAGE-NOT-FOUND (err u103))
(define-constant ERR-THRESHOLD-TOO-LOW (err u104))

;; Configuration constants
(define-constant MIN-ARBITRAGE-THRESHOLD u50) ;; 0.5% minimum threshold
(define-constant MAX-PRICE-STALENESS u1008) ;; 7 days max staleness
(define-constant ARBITRAGE-REWARD-RATE u10) ;; 0.1% reward for reporting

;; Contract state
(define-data-var owner principal tx-sender)
(define-data-var total-arbitrage-alerts uint u0)
(define-data-var arbitrage-reward-pool uint u0)
(define-data-var global-arbitrage-threshold uint u100) ;; 1% default threshold

;; External market price feeds
(define-map external-price-oracles
  (string-ascii 32) ;; exchange/chain identifier
  {
    price-token-x: uint,
    price-token-y: uint,
    last-updated: uint,
    reliability-score: uint,
    active: bool
  }
)

;; Arbitrage opportunities detected
(define-map arbitrage-opportunities
  {pair: (string-ascii 32), opportunity-id: uint}
  {
    local-price: uint,
    external-price: uint,
    price-difference: uint,
    profit-potential: uint,
    detected-block: uint,
    reported-by: principal,
    status: (string-ascii 20)
  }
)

;; Arbitrageur registration and rewards
(define-map registered-arbitrageurs
  principal
  {
    alerts-reported: uint,
    successful-arbitrages: uint,
    total-rewards-earned: uint,
    reputation-score: uint,
    last-activity: uint
  }
)

;; Price monitoring configuration per pair
(define-map monitoring-config
  (string-ascii 32) ;; token pair
  {
    arbitrage-threshold: uint,
    monitoring-enabled: bool,
    external-oracles: (list 5 (string-ascii 32)),
    last-arbitrage-check: uint,
    alert-frequency: uint
  }
)

(define-data-var opportunity-counter uint u0)

;; Initialize arbitrage monitoring for a token pair
(define-public (setup-pair-monitoring 
  (pair (string-ascii 32))
  (threshold uint)
  (oracles (list 5 (string-ascii 32)))
)
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
    (asserts! (>= threshold MIN-ARBITRAGE-THRESHOLD) ERR-THRESHOLD-TOO-LOW)
    (ok (map-set monitoring-config pair
      {
        arbitrage-threshold: threshold,
        monitoring-enabled: true,
        external-oracles: oracles,
        last-arbitrage-check: stacks-block-height,
        alert-frequency: u144 ;; Check every ~1 day
      }))))

;; Register external price oracle
(define-public (register-price-oracle
  (oracle-id (string-ascii 32))
  (initial-price-x uint)
  (initial-price-y uint)
  (reliability-score uint)
)
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
    (asserts! (and (> initial-price-x u0) (> initial-price-y u0)) ERR-INVALID-PRICE)
    (ok (map-set external-price-oracles oracle-id
      {
        price-token-x: initial-price-x,
        price-token-y: initial-price-y,
        last-updated: stacks-block-height,
        reliability-score: reliability-score,
        active: true
      }))))

;; Update external oracle price
(define-public (update-oracle-price
  (oracle-id (string-ascii 32))
  (new-price-x uint)
  (new-price-y uint)
)
  (let (
    (oracle (unwrap! (map-get? external-price-oracles oracle-id) ERR-ORACLE-NOT-FOUND)))
    (asserts! (get active oracle) ERR-ORACLE-NOT-FOUND)
    (asserts! (and (> new-price-x u0) (> new-price-y u0)) ERR-INVALID-PRICE)
    (ok (map-set external-price-oracles oracle-id
      (merge oracle {
        price-token-x: new-price-x,
        price-token-y: new-price-y,
        last-updated: stacks-block-height
      })))))

;; Detect arbitrage opportunity
(define-public (detect-arbitrage-opportunity
  (pair (string-ascii 32))
  (local-price uint)
  (external-oracle-id (string-ascii 32))
)
  (let (
    (config (unwrap! (map-get? monitoring-config pair) ERR-ARBITRAGE-NOT-FOUND))
    (oracle (unwrap! (map-get? external-price-oracles external-oracle-id) ERR-ORACLE-NOT-FOUND))
    (external-price (/ (* (get price-token-x oracle) u1000000) (get price-token-y oracle)))
    (price-diff (if (> local-price external-price)
      (- local-price external-price)
      (- external-price local-price)))
    (profit-percentage (/ (* price-diff u10000) external-price))
    (opportunity-id (var-get opportunity-counter))
    (arbitrageur-data (default-to 
      {alerts-reported: u0, successful-arbitrages: u0, total-rewards-earned: u0, reputation-score: u1000, last-activity: u0}
      (map-get? registered-arbitrageurs tx-sender))))
    
    (asserts! (get monitoring-enabled config) ERR-NOT-AUTHORIZED)
    (asserts! (get active oracle) ERR-ORACLE-NOT-FOUND)
    (asserts! (>= profit-percentage (get arbitrage-threshold config)) ERR-THRESHOLD-TOO-LOW)
    
    ;; Record arbitrage opportunity
    (map-set arbitrage-opportunities {pair: pair, opportunity-id: opportunity-id}
      {
        local-price: local-price,
        external-price: external-price,
        price-difference: price-diff,
        profit-potential: profit-percentage,
        detected-block: stacks-block-height,
        reported-by: tx-sender,
        status: "ACTIVE"
      })
    
    ;; Update arbitrageur stats
    (map-set registered-arbitrageurs tx-sender
      (merge arbitrageur-data {
        alerts-reported: (+ (get alerts-reported arbitrageur-data) u1),
        last-activity: stacks-block-height,
        reputation-score: (+ (get reputation-score arbitrageur-data) u10)
      }))
    
    ;; Increment counters
    (var-set opportunity-counter (+ opportunity-id u1))
    (var-set total-arbitrage-alerts (+ (var-get total-arbitrage-alerts) u1))
    
    ;; Calculate and distribute reward to reporter
    (let ((reward (/ (* profit-percentage ARBITRAGE-REWARD-RATE) u10000)))
      (if (and (> reward u0) (>= (var-get arbitrage-reward-pool) reward))
        (begin
          (var-set arbitrage-reward-pool (- (var-get arbitrage-reward-pool) reward))
          (map-set registered-arbitrageurs tx-sender
            (merge arbitrageur-data {
              total-rewards-earned: (+ (get total-rewards-earned arbitrageur-data) reward)
            }))
          (ok {opportunity-id: opportunity-id, reward-earned: reward}))
        (ok {opportunity-id: opportunity-id, reward-earned: u0})))))

;; Mark arbitrage opportunity as executed
(define-public (mark-arbitrage-executed
  (pair (string-ascii 32))
  (opportunity-id uint)
  (executed-by principal)
)
  (let (
    (opportunity (unwrap! (map-get? arbitrage-opportunities {pair: pair, opportunity-id: opportunity-id}) ERR-ARBITRAGE-NOT-FOUND))
    (arbitrageur-data (default-to 
      {alerts-reported: u0, successful-arbitrages: u0, total-rewards-earned: u0, reputation-score: u1000, last-activity: u0}
      (map-get? registered-arbitrageurs executed-by))))
    
    (asserts! (or (is-eq tx-sender (var-get owner)) (is-eq tx-sender executed-by)) ERR-NOT-AUTHORIZED)
    
    ;; Update opportunity status
    (map-set arbitrage-opportunities {pair: pair, opportunity-id: opportunity-id}
      (merge opportunity {status: "EXECUTED"}))
    
    ;; Reward successful arbitrageur
    (map-set registered-arbitrageurs executed-by
      (merge arbitrageur-data {
        successful-arbitrages: (+ (get successful-arbitrages arbitrageur-data) u1),
        reputation-score: (+ (get reputation-score arbitrageur-data) u50),
        last-activity: stacks-block-height
      }))
    
    (ok true)))

;; Fund the arbitrage reward pool
(define-public (fund-arbitrage-rewards (amount uint))
  (begin
    (var-set arbitrage-reward-pool (+ (var-get arbitrage-reward-pool) amount))
    (ok (var-get arbitrage-reward-pool))))

;; Read-only functions
(define-read-only (get-arbitrage-opportunity (pair (string-ascii 32)) (opportunity-id uint))
  (ok (map-get? arbitrage-opportunities {pair: pair, opportunity-id: opportunity-id})))

(define-read-only (get-arbitrageur-stats (arbitrageur principal))
  (ok (map-get? registered-arbitrageurs arbitrageur)))

(define-read-only (get-monitoring-config (pair (string-ascii 32)))
  (ok (map-get? monitoring-config pair)))

(define-read-only (get-oracle-info (oracle-id (string-ascii 32)))
  (ok (map-get? external-price-oracles oracle-id)))

(define-read-only (calculate-arbitrage-potential
  (local-price uint)
  (external-price uint)
)
  (let (
    (price-diff (if (> local-price external-price)
      (- local-price external-price)
      (- external-price local-price)))
    (profit-percentage (if (> external-price u0)
      (/ (* price-diff u10000) external-price)
      u0)))
    (ok {
      price-difference: price-diff,
      profit-percentage: profit-percentage,
      arbitrage-direction: (if (> local-price external-price) "SELL_LOCAL" "BUY_LOCAL")
    })))

(define-read-only (get-system-stats)
  (ok {
    total-arbitrage-alerts: (var-get total-arbitrage-alerts),
    arbitrage-reward-pool: (var-get arbitrage-reward-pool),
    global-threshold: (var-get global-arbitrage-threshold),
    active-opportunities: (var-get opportunity-counter)
  }))

;; Administrative functions
(define-public (toggle-oracle-status (oracle-id (string-ascii 32)) (active bool))
  (let (
    (oracle (unwrap! (map-get? external-price-oracles oracle-id) ERR-ORACLE-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
    (ok (map-set external-price-oracles oracle-id
      (merge oracle {active: active})))))

(define-public (update-global-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
    (asserts! (>= new-threshold MIN-ARBITRAGE-THRESHOLD) ERR-THRESHOLD-TOO-LOW)
    (ok (var-set global-arbitrage-threshold new-threshold))))

(define-public (toggle-pair-monitoring (pair (string-ascii 32)) (enabled bool))
  (let (
    (config (unwrap! (map-get? monitoring-config pair) ERR-ARBITRAGE-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
    (ok (map-set monitoring-config pair
      (merge config {monitoring-enabled: enabled})))))
