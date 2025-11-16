(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PARTNER-EXISTS (err u101))
(define-constant ERR-PARTNER-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-POINTS (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-USER-NOT-FOUND (err u105))

(define-data-var contract-owner principal tx-sender)

(define-map Partners
    principal
    {
        name: (string-ascii 64),
        points-multiplier: uint,
        active: bool,
    }
)

(define-map UserPoints
    {
        user: principal,
        partner: principal,
    }
    { balance: uint }
)

(define-map TotalPoints
    principal
    { total: uint }
)

(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err u100))
        (ok (var-set contract-owner new-owner))
    )
)

(define-public (register-partner
        (partner-principal principal)
        (partner-name (string-ascii 64))
        (multiplier uint)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err u100))
        (asserts! (is-none (map-get? Partners partner-principal)) (err u101))
        (ok (map-set Partners partner-principal {
            name: partner-name,
            points-multiplier: multiplier,
            active: true,
        }))
    )
)

(define-public (deactivate-partner (partner-principal principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err u100))
        (match (map-get? Partners partner-principal)
            partner-data (ok (map-set Partners partner-principal
                (merge partner-data { active: false })
            ))
            (err u102)
        )
    )
)

(define-public (issue-points
        (user principal)
        (amount uint)
    )
    (let (
            (partner-data (unwrap! (map-get? Partners tx-sender) (err u102)))
            (multiplied-amount (* amount (get points-multiplier partner-data)))
        )
        (asserts! (get active partner-data) (err u100))
        (asserts! (> amount u0) (err u104))
        (unwrap! (add-points user tx-sender multiplied-amount) (err u103))
        (unwrap! (add-to-total user multiplied-amount) (err u103))
        (ok multiplied-amount)
    )
)

(define-public (redeem-points
        (partner principal)
        (amount uint)
    )
    (let (
            (user-points (unwrap!
                (map-get? UserPoints {
                    user: tx-sender,
                    partner: partner,
                })
                (err u105)
            ))
            (partner-data (unwrap! (map-get? Partners partner) (err u102)))
        )
        (asserts! (get active partner-data) (err u100))
        (asserts! (>= (get balance user-points) amount) (err u103))
        (match (deduct-points tx-sender partner amount)
            success1 (match (deduct-from-total tx-sender amount)
                success2 (ok amount)
                error (err u103)
            )
            error (err u103)
        )
    )
)

(define-public (transfer-points
        (recipient principal)
        (partner principal)
        (amount uint)
    )
    (let (
            (sender-points (unwrap!
                (map-get? UserPoints {
                    user: tx-sender,
                    partner: partner,
                })
                (err u105)
            ))
            (partner-data (unwrap! (map-get? Partners partner) (err u102)))
        )
        (asserts! (get active partner-data) (err u100))
        (asserts! (>= (get balance sender-points) amount) (err u103))
        (unwrap! (deduct-points tx-sender partner amount) (err u103))
        (unwrap! (add-points recipient partner amount) (err u103))
        (ok amount)
    )
)
(define-private (add-points
        (user principal)
        (partner principal)
        (amount uint)
    )
    (let ((current-balance (default-to { balance: u0 }
            (map-get? UserPoints {
                user: user,
                partner: partner,
            })
        )))
        (ok (map-set UserPoints {
            user: user,
            partner: partner,
        } { balance: (+ (get balance current-balance) amount) }
        ))
    )
)

(define-private (deduct-points
        (user principal)
        (partner principal)
        (amount uint)
    )
    (let ((current-balance (unwrap!
            (map-get? UserPoints {
                user: user,
                partner: partner,
            })
            (err u105)
        )))
        (ok (map-set UserPoints {
            user: user,
            partner: partner,
        } { balance: (- (get balance current-balance) amount) }
        ))
    )
)

(define-private (add-to-total
        (user principal)
        (amount uint)
    )
    (let ((current-total (default-to { total: u0 } (map-get? TotalPoints user))))
        (ok (map-set TotalPoints user { total: (+ (get total current-total) amount) }))
    )
)

(define-private (deduct-from-total
        (user principal)
        (amount uint)
    )
    (let ((current-total (unwrap! (map-get? TotalPoints user) (err u105))))
        (ok (map-set TotalPoints user { total: (- (get total current-total) amount) }))
    )
)

(define-read-only (get-partner-info (partner principal))
    (ok (unwrap! (map-get? Partners partner) (err u102)))
)

(define-read-only (get-user-points
        (user principal)
        (partner principal)
    )
    (ok (unwrap!
        (map-get? UserPoints {
            user: user,
            partner: partner,
        })
        (err u105)
    ))
)

(define-read-only (get-total-points (user principal))
    (ok (unwrap! (map-get? TotalPoints user) (err u105)))
)

(define-constant ERR-POINTS-EXPIRED (err u106))
(define-constant ERR-INVALID-EXPIRY (err u107))

(define-data-var default-expiry-blocks uint u144000)

(define-map PointsWithExpiry
    {
        user: principal,
        partner: principal,
        batch-id: uint,
    }
    {
        amount: uint,
        expiry-block: uint,
    }
)

(define-constant TIER-BRONZE u0)
(define-constant TIER-SILVER u1)
(define-constant TIER-GOLD u2)
(define-constant TIER-PLATINUM u3)

(define-constant ERR-INVALID-TIER (err u108))
(define-constant ERR-TIER-NOT-QUALIFIED (err u109))

(define-data-var bronze-threshold uint u1000)
(define-data-var silver-threshold uint u5000)
(define-data-var gold-threshold uint u15000)
(define-data-var platinum-threshold uint u50000)

(define-map UserTiers
    principal
    {
        current-tier: uint,
        tier-points: uint,
        tier-multiplier: uint,
    }
)

(define-map TierBenefits
    uint
    {
        name: (string-ascii 32),
        multiplier: uint,
        min-points: uint,
    }
)

(define-public (initialize-tiers)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (map-set TierBenefits TIER-BRONZE {
            name: "Bronze",
            multiplier: u100,
            min-points: u0,
        })
        (map-set TierBenefits TIER-SILVER {
            name: "Silver",
            multiplier: u110,
            min-points: (var-get bronze-threshold),
        })
        (map-set TierBenefits TIER-GOLD {
            name: "Gold",
            multiplier: u125,
            min-points: (var-get silver-threshold),
        })
        (map-set TierBenefits TIER-PLATINUM {
            name: "Platinum",
            multiplier: u150,
            min-points: (var-get gold-threshold),
        })
        (ok true)
    )
)

(define-public (update-tier-thresholds
        (bronze uint)
        (silver uint)
        (gold uint)
        (platinum uint)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (< bronze silver) ERR-INVALID-TIER)
        (asserts! (< silver gold) ERR-INVALID-TIER)
        (asserts! (< gold platinum) ERR-INVALID-TIER)
        (var-set bronze-threshold bronze)
        (var-set silver-threshold silver)
        (var-set gold-threshold gold)
        (var-set platinum-threshold platinum)
        (ok true)
    )
)

(define-public (update-user-tier (user principal))
    (let (
            (total-points-data (unwrap! (map-get? TotalPoints user) ERR-USER-NOT-FOUND))
            (total-points (get total total-points-data))
            (new-tier (calculate-tier total-points))
            (tier-benefits (unwrap! (map-get? TierBenefits new-tier) ERR-INVALID-TIER))
        )
        (map-set UserTiers user {
            current-tier: new-tier,
            tier-points: total-points,
            tier-multiplier: (get multiplier tier-benefits),
        })
        (ok new-tier)
    )
)

(define-public (issue-points-with-tier-bonus
        (user principal)
        (amount uint)
    )
    (let (
            (partner-data (unwrap! (map-get? Partners tx-sender) ERR-PARTNER-NOT-FOUND))
            (user-tier (default-to {
                current-tier: TIER-BRONZE,
                tier-points: u0,
                tier-multiplier: u100,
            }
                (map-get? UserTiers user)
            ))
            (base-amount (* amount (get points-multiplier partner-data)))
            (tier-bonus (/ (* base-amount (get tier-multiplier user-tier)) u100))
            (final-amount (+ base-amount tier-bonus))
        )
        (asserts! (get active partner-data) ERR-NOT-AUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (unwrap! (add-points user tx-sender final-amount) ERR-INSUFFICIENT-POINTS)
        (unwrap! (add-to-total user final-amount) ERR-INSUFFICIENT-POINTS)
        (unwrap! (update-user-tier user) ERR-INVALID-TIER)
        (ok final-amount)
    )
)

(define-public (claim-tier-upgrade (target-tier uint))
    (let (
            (total-points-data (unwrap! (map-get? TotalPoints tx-sender) ERR-USER-NOT-FOUND))
            (total-points (get total total-points-data))
            (qualified-tier (calculate-tier total-points))
            (tier-benefits (unwrap! (map-get? TierBenefits target-tier) ERR-INVALID-TIER))
        )
        (asserts! (>= qualified-tier target-tier) ERR-TIER-NOT-QUALIFIED)
        (map-set UserTiers tx-sender {
            current-tier: target-tier,
            tier-points: total-points,
            tier-multiplier: (get multiplier tier-benefits),
        })
        (ok target-tier)
    )
)

(define-private (calculate-tier (points uint))
    (if (>= points (var-get platinum-threshold))
        TIER-PLATINUM
        (if (>= points (var-get gold-threshold))
            TIER-GOLD
            (if (>= points (var-get silver-threshold))
                TIER-SILVER
                TIER-BRONZE
            )
        )
    )
)

(define-read-only (get-user-tier (user principal))
    (ok (default-to {
        current-tier: TIER-BRONZE,
        tier-points: u0,
        tier-multiplier: u100,
    }
        (map-get? UserTiers user)
    ))
)

(define-read-only (get-tier-benefits (tier uint))
    (ok (unwrap! (map-get? TierBenefits tier) ERR-INVALID-TIER))
)

(define-read-only (get-tier-thresholds)
    (ok {
        bronze: (var-get bronze-threshold),
        silver: (var-get silver-threshold),
        gold: (var-get gold-threshold),
        platinum: (var-get platinum-threshold),
    })
)

(define-read-only (calculate-user-tier (user principal))
    (let (
            (total-points-data (unwrap! (map-get? TotalPoints user) ERR-USER-NOT-FOUND))
            (total-points (get total total-points-data))
        )
        (ok (calculate-tier total-points))
    )
)

(define-map UserBatchCounters
    {
        user: principal,
        partner: principal,
    }
    { counter: uint }
)

(define-public (set-default-expiry (blocks uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> blocks u0) ERR-INVALID-EXPIRY)
        (ok (var-set default-expiry-blocks blocks))
    )
)

(define-public (issue-points-with-expiry
        (user principal)
        (amount uint)
        (expiry-blocks uint)
    )
    (let (
            (partner-data (unwrap! (map-get? Partners tx-sender) ERR-PARTNER-NOT-FOUND))
            (multiplied-amount (* amount (get points-multiplier partner-data)))
            (batch-counter (get-next-batch-id user tx-sender))
            (expiry-block (+ stacks-block-height expiry-blocks))
        )
        (asserts! (get active partner-data) ERR-NOT-AUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> expiry-blocks u0) ERR-INVALID-EXPIRY)
        (map-set PointsWithExpiry {
            user: user,
            partner: tx-sender,
            batch-id: batch-counter,
        } {
            amount: multiplied-amount,
            expiry-block: expiry-block,
        })
        (unwrap! (add-points user tx-sender multiplied-amount)
            ERR-INSUFFICIENT-POINTS
        )
        (unwrap! (add-to-total user multiplied-amount) ERR-INSUFFICIENT-POINTS)
        (ok multiplied-amount)
    )
)

(define-public (cleanup-expired-points
        (user principal)
        (partner principal)
        (batch-id uint)
    )
    (let ((points-data (unwrap!
            (map-get? PointsWithExpiry {
                user: user,
                partner: partner,
                batch-id: batch-id,
            })
            ERR-USER-NOT-FOUND
        )))
        (asserts! (>= stacks-block-height (get expiry-block points-data))
            ERR-POINTS-EXPIRED
        )
        (map-delete PointsWithExpiry {
            user: user,
            partner: partner,
            batch-id: batch-id,
        })
        (unwrap! (deduct-points user partner (get amount points-data))
            ERR-INSUFFICIENT-POINTS
        )
        (unwrap! (deduct-from-total user (get amount points-data))
            ERR-INSUFFICIENT-POINTS
        )
        (ok (get amount points-data))
    )
)

(define-private (get-next-batch-id
        (user principal)
        (partner principal)
    )
    (let (
            (current-counter (default-to { counter: u0 }
                (map-get? UserBatchCounters {
                    user: user,
                    partner: partner,
                })
            ))
            (next-counter (+ (get counter current-counter) u1))
        )
        (map-set UserBatchCounters {
            user: user,
            partner: partner,
        } { counter: next-counter }
        )
        next-counter
    )
)

(define-read-only (get-points-expiry
        (user principal)
        (partner principal)
        (batch-id uint)
    )
    (ok (unwrap!
        (map-get? PointsWithExpiry {
            user: user,
            partner: partner,
            batch-id: batch-id,
        })
        ERR-USER-NOT-FOUND
    ))
)

(define-read-only (check-points-expired
        (user principal)
        (partner principal)
        (batch-id uint)
    )
    (let ((points-data (unwrap!
            (map-get? PointsWithExpiry {
                user: user,
                partner: partner,
                batch-id: batch-id,
            })
            ERR-USER-NOT-FOUND
        )))
        (ok (>= stacks-block-height (get expiry-block points-data)))
    )
)

(define-constant ERR-REWARD-NOT-FOUND (err u110))
(define-constant ERR-REWARD-INACTIVE (err u111))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u112))
(define-constant ERR-INSUFFICIENT-BURN-POINTS (err u113))

(define-data-var reward-id-counter uint u0)

(define-map BurnRewards
    uint
    {
        partner: principal,
        title: (string-ascii 64),
        burn-cost: uint,
        max-claims: uint,
        current-claims: uint,
        active: bool,
        tier-requirement: uint,
    }
)

(define-map UserBurnClaims
    {
        user: principal,
        reward-id: uint,
    }
    { claimed: bool }
)

(define-map PartnerBurnStats
    principal
    {
        total-burns: uint,
        total-rewards-created: uint,
    }
)

(define-map UserBurnHistory
    principal
    {
        total-burned: uint,
        rewards-claimed: uint,
    }
)

(define-public (create-burn-reward
        (title (string-ascii 64))
        (burn-cost uint)
        (max-claims uint)
        (tier-requirement uint)
    )
    (let (
            (reward-id (+ (var-get reward-id-counter) u1))
            (partner-data (unwrap! (map-get? Partners tx-sender) ERR-PARTNER-NOT-FOUND))
        )
        (asserts! (get active partner-data) ERR-NOT-AUTHORIZED)
        (asserts! (> burn-cost u0) ERR-INVALID-AMOUNT)
        (asserts! (> max-claims u0) ERR-INVALID-AMOUNT)
        (asserts! (<= tier-requirement TIER-PLATINUM) ERR-INVALID-TIER)
        (map-set BurnRewards reward-id {
            partner: tx-sender,
            title: title,
            burn-cost: burn-cost,
            max-claims: max-claims,
            current-claims: u0,
            active: true,
            tier-requirement: tier-requirement,
        })
        (let ((current-stats (default-to {
                total-burns: u0,
                total-rewards-created: u0,
            }
                (map-get? PartnerBurnStats tx-sender)
            )))
            (map-set PartnerBurnStats tx-sender
                (merge current-stats { total-rewards-created: (+ (get total-rewards-created current-stats) u1) })
            )
        )
        (var-set reward-id-counter reward-id)
        (ok reward-id)
    )
)

(define-public (claim-burn-reward (reward-id uint))
    (let (
            (reward-data (unwrap! (map-get? BurnRewards reward-id) ERR-REWARD-NOT-FOUND))
            (user-tier-data (default-to {
                current-tier: TIER-BRONZE,
                tier-points: u0,
                tier-multiplier: u100,
            }
                (map-get? UserTiers tx-sender)
            ))
            (user-points (unwrap!
                (map-get? UserPoints {
                    user: tx-sender,
                    partner: (get partner reward-data),
                })
                ERR-USER-NOT-FOUND
            ))
            (already-claimed (default-to { claimed: false }
                (map-get? UserBurnClaims {
                    user: tx-sender,
                    reward-id: reward-id,
                })
            ))
        )
        (asserts! (get active reward-data) ERR-REWARD-INACTIVE)
        (asserts! (not (get claimed already-claimed)) ERR-REWARD-ALREADY-CLAIMED)
        (asserts!
            (< (get current-claims reward-data) (get max-claims reward-data))
            ERR-REWARD-INACTIVE
        )
        (asserts!
            (>= (get current-tier user-tier-data)
                (get tier-requirement reward-data)
            )
            ERR-TIER-NOT-QUALIFIED
        )
        (asserts! (>= (get balance user-points) (get burn-cost reward-data))
            ERR-INSUFFICIENT-BURN-POINTS
        )
        (unwrap!
            (deduct-points tx-sender (get partner reward-data)
                (get burn-cost reward-data)
            )
            ERR-INSUFFICIENT-POINTS
        )
        (unwrap! (deduct-from-total tx-sender (get burn-cost reward-data))
            ERR-INSUFFICIENT-POINTS
        )
        (map-set BurnRewards reward-id
            (merge reward-data { current-claims: (+ (get current-claims reward-data) u1) })
        )
        (map-set UserBurnClaims {
            user: tx-sender,
            reward-id: reward-id,
        } { claimed: true }
        )
        (let (
                (partner-stats (default-to {
                    total-burns: u0,
                    total-rewards-created: u0,
                }
                    (map-get? PartnerBurnStats (get partner reward-data))
                ))
                (user-history (default-to {
                    total-burned: u0,
                    rewards-claimed: u0,
                }
                    (map-get? UserBurnHistory tx-sender)
                ))
            )
            (map-set PartnerBurnStats (get partner reward-data)
                (merge partner-stats { total-burns: (+ (get total-burns partner-stats) (get burn-cost reward-data)) })
            )
            (map-set UserBurnHistory tx-sender {
                total-burned: (+ (get total-burned user-history) (get burn-cost reward-data)),
                rewards-claimed: (+ (get rewards-claimed user-history) u1),
            })
        )
        (ok reward-id)
    )
)

(define-public (deactivate-burn-reward (reward-id uint))
    (let ((reward-data (unwrap! (map-get? BurnRewards reward-id) ERR-REWARD-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get partner reward-data)) ERR-NOT-AUTHORIZED)
        (map-set BurnRewards reward-id (merge reward-data { active: false }))
        (ok true)
    )
)

(define-public (bulk-burn-points
        (partner principal)
        (amount uint)
    )
    (let (
            (user-points (unwrap!
                (map-get? UserPoints {
                    user: tx-sender,
                    partner: partner,
                })
                ERR-USER-NOT-FOUND
            ))
            (partner-data (unwrap! (map-get? Partners partner) ERR-PARTNER-NOT-FOUND))
        )
        (asserts! (get active partner-data) ERR-NOT-AUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (>= (get balance user-points) amount)
            ERR-INSUFFICIENT-BURN-POINTS
        )
        (unwrap! (deduct-points tx-sender partner amount) ERR-INSUFFICIENT-POINTS)
        (unwrap! (deduct-from-total tx-sender amount) ERR-INSUFFICIENT-POINTS)
        (let (
                (partner-stats (default-to {
                    total-burns: u0,
                    total-rewards-created: u0,
                }
                    (map-get? PartnerBurnStats partner)
                ))
                (user-history (default-to {
                    total-burned: u0,
                    rewards-claimed: u0,
                }
                    (map-get? UserBurnHistory tx-sender)
                ))
            )
            (map-set PartnerBurnStats partner
                (merge partner-stats { total-burns: (+ (get total-burns partner-stats) amount) })
            )
            (map-set UserBurnHistory tx-sender
                (merge user-history { total-burned: (+ (get total-burned user-history) amount) })
            )
        )
        (ok amount)
    )
)

(define-read-only (get-burn-reward-info (reward-id uint))
    (ok (unwrap! (map-get? BurnRewards reward-id) ERR-REWARD-NOT-FOUND))
)

(define-read-only (get-user-burn-claim-status
        (user principal)
        (reward-id uint)
    )
    (ok (default-to { claimed: false }
        (map-get? UserBurnClaims {
            user: user,
            reward-id: reward-id,
        })
    ))
)

(define-read-only (get-partner-burn-stats (partner principal))
    (ok (default-to {
        total-burns: u0,
        total-rewards-created: u0,
    }
        (map-get? PartnerBurnStats partner)
    ))
)

(define-read-only (get-user-burn-history (user principal))
    (ok (default-to {
        total-burned: u0,
        rewards-claimed: u0,
    }
        (map-get? UserBurnHistory user)
    ))
)

(define-read-only (get-current-reward-id)
    (ok (var-get reward-id-counter))
)

(define-constant ERR-GIFT-SELF (err u114))
(define-constant ERR-GIFT-LIMIT-EXCEEDED (err u115))
(define-constant ERR-DAILY-LIMIT-EXCEEDED (err u116))

(define-data-var daily-gift-limit uint u1000)
(define-data-var max-gift-amount uint u500)

(define-map GiftTransactions
    uint
    {
        sender: principal,
        recipient: principal,
        partner: principal,
        amount: uint,
        message: (string-ascii 128),
        timestamp: uint,
    }
)

(define-map UserGiftStats
    principal
    {
        total-sent: uint,
        total-received: uint,
        gifts-sent-count: uint,
        gifts-received-count: uint,
    }
)

(define-map DailyGiftLimits
    {
        user: principal,
        day: uint,
    }
    { amount-sent: uint }
)

(define-data-var gift-id-counter uint u0)

(define-public (set-gift-limits
        (daily-limit uint)
        (max-amount uint)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> daily-limit u0) ERR-INVALID-AMOUNT)
        (asserts! (> max-amount u0) ERR-INVALID-AMOUNT)
        (var-set daily-gift-limit daily-limit)
        (var-set max-gift-amount max-amount)
        (ok true)
    )
)

(define-public (gift-points
        (recipient principal)
        (partner principal)
        (amount uint)
        (message (string-ascii 128))
    )
    (let (
            (sender-points (unwrap!
                (map-get? UserPoints {
                    user: tx-sender,
                    partner: partner,
                })
                ERR-USER-NOT-FOUND
            ))
            (partner-data (unwrap! (map-get? Partners partner) ERR-PARTNER-NOT-FOUND))
            (current-day (/ stacks-block-height u144))
            (daily-sent (default-to { amount-sent: u0 }
                (map-get? DailyGiftLimits {
                    user: tx-sender,
                    day: current-day,
                })
            ))
            (gift-id (+ (var-get gift-id-counter) u1))
        )
        (asserts! (not (is-eq tx-sender recipient)) ERR-GIFT-SELF)
        (asserts! (get active partner-data) ERR-NOT-AUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (<= amount (var-get max-gift-amount)) ERR-GIFT-LIMIT-EXCEEDED)
        (asserts! (>= (get balance sender-points) amount) ERR-INSUFFICIENT-POINTS)
        (asserts!
            (<= (+ (get amount-sent daily-sent) amount)
                (var-get daily-gift-limit)
            )
            ERR-DAILY-LIMIT-EXCEEDED
        )
        (unwrap! (deduct-points tx-sender partner amount) ERR-INSUFFICIENT-POINTS)
        (unwrap! (add-points recipient partner amount) ERR-INSUFFICIENT-POINTS)
        (map-set GiftTransactions gift-id {
            sender: tx-sender,
            recipient: recipient,
            partner: partner,
            amount: amount,
            message: message,
            timestamp: stacks-block-height,
        })
        (map-set DailyGiftLimits {
            user: tx-sender,
            day: current-day,
        } { amount-sent: (+ (get amount-sent daily-sent) amount) }
        )
        (let (
                (sender-stats (default-to {
                    total-sent: u0,
                    total-received: u0,
                    gifts-sent-count: u0,
                    gifts-received-count: u0,
                }
                    (map-get? UserGiftStats tx-sender)
                ))
                (recipient-stats (default-to {
                    total-sent: u0,
                    total-received: u0,
                    gifts-sent-count: u0,
                    gifts-received-count: u0,
                }
                    (map-get? UserGiftStats recipient)
                ))
            )
            (map-set UserGiftStats tx-sender {
                total-sent: (+ (get total-sent sender-stats) amount),
                total-received: (get total-received sender-stats),
                gifts-sent-count: (+ (get gifts-sent-count sender-stats) u1),
                gifts-received-count: (get gifts-received-count sender-stats),
            })
            (map-set UserGiftStats recipient {
                total-sent: (get total-sent recipient-stats),
                total-received: (+ (get total-received recipient-stats) amount),
                gifts-sent-count: (get gifts-sent-count recipient-stats),
                gifts-received-count: (+ (get gifts-received-count recipient-stats) u1),
            })
        )
        (var-set gift-id-counter gift-id)
        (ok gift-id)
    )
)

(define-read-only (get-gift-transaction (gift-id uint))
    (ok (unwrap! (map-get? GiftTransactions gift-id) ERR-REWARD-NOT-FOUND))
)

(define-read-only (get-user-gift-stats (user principal))
    (ok (default-to {
        total-sent: u0,
        total-received: u0,
        gifts-sent-count: u0,
        gifts-received-count: u0,
    }
        (map-get? UserGiftStats user)
    ))
)

(define-read-only (get-daily-gift-remaining (user principal))
    (let (
            (current-day (/ stacks-block-height u144))
            (daily-sent (default-to { amount-sent: u0 }
                (map-get? DailyGiftLimits {
                    user: user,
                    day: current-day,
                })
            ))
        )
        (ok (- (var-get daily-gift-limit) (get amount-sent daily-sent)))
    )
)

(define-read-only (get-gift-limits)
    (ok {
        daily-limit: (var-get daily-gift-limit),
        max-amount: (var-get max-gift-amount),
    })
)

(define-read-only (get-current-gift-id)
    (ok (var-get gift-id-counter))
)

(define-constant ERR-ALREADY-REFERRED (err u117))
(define-constant ERR-SELF-REFERRAL (err u118))
(define-constant ERR-NO-REFERRER (err u119))
(define-constant ERR-REFERRAL-INACTIVE (err u120))

(define-data-var referral-bonus-percentage uint u10)
(define-data-var referral-system-active bool true)
(define-data-var max-referral-bonus uint u100)

(define-map UserReferrals
    principal
    {
        referrer: principal,
        referral-code: uint,
        total-referrals: uint,
        total-bonus-earned: uint,
        active: bool,
    }
)

(define-map ReferralEarnings
    {
        referrer: principal,
        partner: principal,
    }
    { earnings: uint }
)

(define-map ReferralActivity
    principal
    {
        referee-count: uint,
        lifetime-earnings: uint,
        last-bonus-block: uint,
    }
)

(define-public (set-referral-bonus-percentage (percentage uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= percentage u100) ERR-INVALID-AMOUNT)
        (var-set referral-bonus-percentage percentage)
        (ok true)
    )
)

(define-public (toggle-referral-system (active bool))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set referral-system-active active)
        (ok true)
    )
)

(define-public (set-max-referral-bonus (max-bonus uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> max-bonus u0) ERR-INVALID-AMOUNT)
        (var-set max-referral-bonus max-bonus)
        (ok true)
    )
)

(define-public (register-referral (referrer principal))
    (let (
            (existing-referral (map-get? UserReferrals tx-sender))
            (referrer-data (default-to {
                referrer: tx-sender,
                referral-code: u0,
                total-referrals: u0,
                total-bonus-earned: u0,
                active: true,
            }
                (map-get? UserReferrals referrer)
            ))
            (referrer-activity (default-to {
                referee-count: u0,
                lifetime-earnings: u0,
                last-bonus-block: u0,
            }
                (map-get? ReferralActivity referrer)
            ))
        )
        (asserts! (var-get referral-system-active) ERR-REFERRAL-INACTIVE)
        (asserts! (is-none existing-referral) ERR-ALREADY-REFERRED)
        (asserts! (not (is-eq tx-sender referrer)) ERR-SELF-REFERRAL)
        (map-set UserReferrals tx-sender {
            referrer: referrer,
            referral-code: (+ (get referee-count referrer-activity) u1),
            total-referrals: u0,
            total-bonus-earned: u0,
            active: true,
        })
        (map-set ReferralActivity referrer {
            referee-count: (+ (get referee-count referrer-activity) u1),
            lifetime-earnings: (get lifetime-earnings referrer-activity),
            last-bonus-block: stacks-block-height,
        })
        (ok true)
    )
)

(define-public (issue-points-with-referral-bonus
        (user principal)
        (amount uint)
    )
    (let (
            (partner-data (unwrap! (map-get? Partners tx-sender) ERR-PARTNER-NOT-FOUND))
            (multiplied-amount (* amount (get points-multiplier partner-data)))
            (referee-data (map-get? UserReferrals user))
        )
        (asserts! (get active partner-data) ERR-NOT-AUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (unwrap! (add-points user tx-sender multiplied-amount)
            ERR-INSUFFICIENT-POINTS
        )
        (unwrap! (add-to-total user multiplied-amount) ERR-INSUFFICIENT-POINTS)
        (match referee-data
            referral-info (let (
                    (referrer (get referrer referral-info))
                    (bonus-amount (/ (* multiplied-amount (var-get referral-bonus-percentage))
                        u100
                    ))
                    (capped-bonus (if (<= bonus-amount (var-get max-referral-bonus))
                        bonus-amount
                        (var-get max-referral-bonus)
                    ))
                )
                (if (and
                        (get active referral-info)
                        (var-get referral-system-active)
                        (> capped-bonus u0)
                    )
                    (begin
                        (unwrap! (add-points referrer tx-sender capped-bonus)
                            ERR-INSUFFICIENT-POINTS
                        )
                        (unwrap! (add-to-total referrer capped-bonus)
                            ERR-INSUFFICIENT-POINTS
                        )
                        (let (
                                (referrer-earnings (default-to { earnings: u0 }
                                    (map-get? ReferralEarnings {
                                        referrer: referrer,
                                        partner: tx-sender,
                                    })
                                ))
                                (referrer-activity (default-to {
                                    referee-count: u0,
                                    lifetime-earnings: u0,
                                    last-bonus-block: u0,
                                }
                                    (map-get? ReferralActivity referrer)
                                ))
                                (updated-referral (merge referral-info { total-bonus-earned: (+ (get total-bonus-earned referral-info)
                                    capped-bonus
                                ) }
                                ))
                            )
                            (map-set ReferralEarnings {
                                referrer: referrer,
                                partner: tx-sender,
                            } { earnings: (+ (get earnings referrer-earnings) capped-bonus) }
                            )
                            (map-set ReferralActivity referrer {
                                referee-count: (get referee-count referrer-activity),
                                lifetime-earnings: (+ (get lifetime-earnings referrer-activity)
                                    capped-bonus
                                ),
                                last-bonus-block: stacks-block-height,
                            })
                            (map-set UserReferrals user updated-referral)
                            (ok {
                                amount: multiplied-amount,
                                bonus: capped-bonus,
                            })
                        )
                    )
                    (ok {
                        amount: multiplied-amount,
                        bonus: u0,
                    })
                )
            )
            (ok {
                amount: multiplied-amount,
                bonus: u0,
            })
        )
    )
)

(define-public (deactivate-referral)
    (let ((referral-data (unwrap! (map-get? UserReferrals tx-sender) ERR-NO-REFERRER)))
        (map-set UserReferrals tx-sender (merge referral-data { active: false }))
        (ok true)
    )
)

(define-public (reactivate-referral)
    (let ((referral-data (unwrap! (map-get? UserReferrals tx-sender) ERR-NO-REFERRER)))
        (asserts! (var-get referral-system-active) ERR-REFERRAL-INACTIVE)
        (map-set UserReferrals tx-sender (merge referral-data { active: true }))
        (ok true)
    )
)

(define-read-only (get-referral-info (user principal))
    (ok (unwrap! (map-get? UserReferrals user) ERR-NO-REFERRER))
)

(define-read-only (get-referral-activity (referrer principal))
    (ok (default-to {
        referee-count: u0,
        lifetime-earnings: u0,
        last-bonus-block: u0,
    }
        (map-get? ReferralActivity referrer)
    ))
)

(define-read-only (get-referral-earnings
        (referrer principal)
        (partner principal)
    )
    (ok (default-to { earnings: u0 }
        (map-get? ReferralEarnings {
            referrer: referrer,
            partner: partner,
        })
    ))
)

(define-read-only (get-referral-settings)
    (ok {
        bonus-percentage: (var-get referral-bonus-percentage),
        system-active: (var-get referral-system-active),
        max-bonus: (var-get max-referral-bonus),
    })
)

(define-read-only (check-referral-eligibility (user principal))
    (ok (is-none (map-get? UserReferrals user)))
)

(define-read-only (get-user-overview (user principal))
    (let (
            (total-points-data (default-to { total: u0 } (map-get? TotalPoints user)))
            (total-points (get total total-points-data))
            (tier-data (default-to {
                current-tier: TIER-BRONZE,
                tier-points: u0,
                tier-multiplier: u100,
            }
                (map-get? UserTiers user)
            ))
            (burn-history (default-to {
                total-burned: u0,
                rewards-claimed: u0,
            }
                (map-get? UserBurnHistory user)
            ))
            (gift-stats (default-to {
                total-sent: u0,
                total-received: u0,
                gifts-sent-count: u0,
                gifts-received-count: u0,
            }
                (map-get? UserGiftStats user)
            ))
            (referral-info (map-get? UserReferrals user))
            (referral-activity (default-to {
                referee-count: u0,
                lifetime-earnings: u0,
                last-bonus-block: u0,
            }
                (map-get? ReferralActivity user)
            ))
        )
        (ok {
            total-points: total-points,
            tier: tier-data,
            burn: burn-history,
            gifts: gift-stats,
            referral: (default-to {
                referrer: user,
                referral-code: u0,
                total-referrals: u0,
                total-bonus-earned: u0,
                active: false,
            }
                referral-info
            ),
            referral-activity: referral-activity,
        })
    )
)
