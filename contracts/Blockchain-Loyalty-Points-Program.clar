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
