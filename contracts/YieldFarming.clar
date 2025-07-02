(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-FARM-NOT-FOUND (err u107))
(define-constant ERR-NOT-ENROLLED (err u108))
(define-constant ERR-FARM-ENDED (err u109))

(define-data-var farm-counter uint u0)
(define-data-var total-farming-rewards uint u0)
(define-data-var owner principal tx-sender)

(define-map farming-pools
    {farm-id: uint}
    {
        token-pair: (string-ascii 32),
        reward-rate: uint,
        start-block: uint,
        end-block: uint,
        total-staked: uint,
        active: bool
    }
)

(define-map farmer-positions
    {farm-id: uint, farmer: principal}
    {
        staked-amount: uint,
        join-block: uint,
        last-harvest: uint,
        duration-tier: uint,
        multiplier: uint
    }
)

(define-constant DURATION-TIERS
    {
        tier-1: {blocks: u1440, multiplier: u100},
        tier-2: {blocks: u4320, multiplier: u125},
        tier-3: {blocks: u8640, multiplier: u150},
        tier-4: {blocks: u17280, multiplier: u200}
    }
)

(define-public (create-farm 
    (token-pair (string-ascii 32))
    (reward-rate uint)
    (start-block uint)
    (end-block uint)
)
    (let
        (
            (farm-id (var-get farm-counter))
        )
        (begin
            (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
            (asserts! (> end-block start-block) ERR-NOT-AUTHORIZED)
            (var-set farm-counter (+ farm-id u1))
            (ok (map-set farming-pools
                {farm-id: farm-id}
                {
                    token-pair: token-pair,
                    reward-rate: reward-rate,
                    start-block: start-block,
                    end-block: end-block,
                    total-staked: u0,
                    active: true
                }
            ))
        )
    )
)

(define-private (get-duration-tier (blocks-farmed uint))
    (if (>= blocks-farmed (get blocks (get tier-4 DURATION-TIERS)))
        {tier: u4, multiplier: (get multiplier (get tier-4 DURATION-TIERS))}
        (if (>= blocks-farmed (get blocks (get tier-3 DURATION-TIERS)))
            {tier: u3, multiplier: (get multiplier (get tier-3 DURATION-TIERS))}
            (if (>= blocks-farmed (get blocks (get tier-2 DURATION-TIERS)))
                {tier: u2, multiplier: (get multiplier (get tier-2 DURATION-TIERS))}
                {tier: u1, multiplier: (get multiplier (get tier-1 DURATION-TIERS))}
            )
        )
    )
)

(define-public (enroll-in-farm (farm-id uint) (amount uint))
    (let
        (
            (farm (unwrap! (map-get? farming-pools {farm-id: farm-id}) ERR-FARM-NOT-FOUND))
            (current-block stacks-block-height)
        )
        (begin
            (asserts! (get active farm) ERR-FARM-ENDED)
            (asserts! (>= current-block (get start-block farm)) ERR-NOT-AUTHORIZED)
            (asserts! (< current-block (get end-block farm)) ERR-FARM-ENDED)
            (asserts! (> amount u0) ERR-INSUFFICIENT-BALANCE)
            (map-set farmer-positions
                {farm-id: farm-id, farmer: tx-sender}
                {
                    staked-amount: amount,
                    join-block: current-block,
                    last-harvest: current-block,
                    duration-tier: u1,
                    multiplier: (get multiplier (get tier-1 DURATION-TIERS))
                }
            )
            (map-set farming-pools
                {farm-id: farm-id}
                (merge farm {total-staked: (+ (get total-staked farm) amount)})
            )
            (ok true)
        )
    )
)

(define-private (calculate-farming-rewards (farm-id uint) (farmer principal))
    (let
        (
            (farm (unwrap! (map-get? farming-pools {farm-id: farm-id}) (err u0)))
            (position (unwrap! (map-get? farmer-positions {farm-id: farm-id, farmer: farmer}) (err u0)))
            (current-block stacks-block-height)
            (effective-end (if (< current-block (get end-block farm))
                              current-block
                              (get end-block farm)))
            (blocks-since-harvest (- effective-end (get last-harvest position)))
            (blocks-farmed (- current-block (get join-block position)))
            (tier-info (get-duration-tier blocks-farmed))
            (base-rewards (/ (* blocks-since-harvest 
                               (get reward-rate farm) 
                               (get staked-amount position)) 
                            (get total-staked farm)))
            (boosted-rewards (/ (* base-rewards (get multiplier tier-info)) u100))
        )
        (ok {
            rewards: boosted-rewards,
            new-tier: (get tier tier-info),
            new-multiplier: (get multiplier tier-info)
        })
    )
)

(define-public (harvest-rewards (farm-id uint))
    (let
        (
            (farm (unwrap! (map-get? farming-pools {farm-id: farm-id}) ERR-FARM-NOT-FOUND))
            (position (unwrap! (map-get? farmer-positions {farm-id: farm-id, farmer: tx-sender}) ERR-NOT-ENROLLED))
            (reward-info (unwrap! (calculate-farming-rewards farm-id tx-sender) ERR-NOT-ENROLLED))
            (current-block stacks-block-height)
        )
        (begin
            (map-set farmer-positions
                {farm-id: farm-id, farmer: tx-sender}
                (merge position 
                    {
                        last-harvest: current-block,
                        duration-tier: (get new-tier reward-info),
                        multiplier: (get new-multiplier reward-info)
                    }
                )
            )
            (ok (get rewards reward-info))
        )
    )
)

(define-public (compound-rewards (farm-id uint))
    (let
        (
            (reward-info (unwrap! (calculate-farming-rewards farm-id tx-sender) ERR-NOT-ENROLLED))
            (position (unwrap! (map-get? farmer-positions {farm-id: farm-id, farmer: tx-sender}) ERR-NOT-ENROLLED))
            (farm (unwrap! (map-get? farming-pools {farm-id: farm-id}) ERR-FARM-NOT-FOUND))
            (rewards (get rewards reward-info))
            (current-block stacks-block-height)
        )
        (begin
            (map-set farmer-positions
                {farm-id: farm-id, farmer: tx-sender}
                (merge position 
                    {
                        staked-amount: (+ (get staked-amount position) rewards),
                        last-harvest: current-block,
                        duration-tier: (get new-tier reward-info),
                        multiplier: (get new-multiplier reward-info)
                    }
                )
            )
            (map-set farming-pools
                {farm-id: farm-id}
                (merge farm {total-staked: (+ (get total-staked farm) rewards)})
            )
            (ok rewards)
        )
    )
)

(define-public (exit-farm (farm-id uint))
    (let
        (
            (position (unwrap! (map-get? farmer-positions {farm-id: farm-id, farmer: tx-sender}) ERR-NOT-ENROLLED))
            (farm (unwrap! (map-get? farming-pools {farm-id: farm-id}) ERR-FARM-NOT-FOUND))
            (final-rewards (unwrap! (calculate-farming-rewards farm-id tx-sender) ERR-NOT-ENROLLED))
        )
        (begin
            (map-delete farmer-positions {farm-id: farm-id, farmer: tx-sender})
            (map-set farming-pools
                {farm-id: farm-id}
                (merge farm {total-staked: (- (get total-staked farm) (get staked-amount position))})
            )
            (ok {
                withdrawn-amount: (get staked-amount position),
                final-rewards: (get rewards final-rewards)
            })
        )
    )
)

(define-read-only (get-farm-info (farm-id uint))
    (ok (map-get? farming-pools {farm-id: farm-id}))
)

(define-read-only (get-farmer-position (farm-id uint) (farmer principal))
    (ok (map-get? farmer-positions {farm-id: farm-id, farmer: farmer}))
)

(define-read-only (get-pending-rewards (farm-id uint) (farmer principal))
    (calculate-farming-rewards farm-id farmer)
)

(define-public (emergency-close-farm (farm-id uint))
    (let
        (
            (farm (unwrap! (map-get? farming-pools {farm-id: farm-id}) ERR-FARM-NOT-FOUND))
        )
        (begin
            (asserts! (is-eq tx-sender (var-get owner)) ERR-NOT-AUTHORIZED)
            (ok (map-set farming-pools
                {farm-id: farm-id}
                (merge farm {active: false})
            ))
        )
    )
)
