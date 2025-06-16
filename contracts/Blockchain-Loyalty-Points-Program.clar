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
