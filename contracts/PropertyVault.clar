;; PropertyVault: Decentralized Real Estate Investment Platform
;; Version: 1.0.0
;; A protocol for fractional real estate investment, property management, and rental income distribution

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-PROPERTY-NOT-FOUND (err u2))
(define-constant ERR-INVALID-INVESTMENT (err u3))
(define-constant ERR-INVALID-LEASE-TERM (err u4))
(define-constant ERR-INVALID-ADDRESS (err u5))
(define-constant ERR-INVALID-DETAILS (err u6))
(define-constant ERR-PROPERTY-INACTIVE (err u7))
(define-constant ERR-ALREADY-INVESTED (err u8))
(define-constant ERR-NOT-INVESTED (err u9))
(define-constant ERR-INSUFFICIENT-FUNDS (err u10))
(define-constant ERR-PAYOUT-NOT-READY (err u11))
(define-constant ERR-ALREADY-PAID (err u12))
(define-constant ERR-INVALID-PROPERTY-TYPE (err u13))
(define-constant ERR-INVALID-LOCATION-TYPE (err u14))
(define-constant ERR-LEASE-ACTIVE (err u15))
(define-constant ERR-INVALID-AMOUNT (err u16))

;; Constants
(define-constant MIN-INVESTMENT u1000000) ;; 1 STX minimum
(define-constant MAX-INVESTMENT u1000000000000) ;; 1M STX maximum
(define-constant MIN-LEASE-TERM u2592000) ;; 30 days minimum
(define-constant MAX-LEASE-TERM u31536000) ;; 1 year maximum
(define-constant MANAGEMENT-FEE-PERCENT u5) ;; 5% management fee
(define-constant PAYOUT-THRESHOLD u90) ;; 90% minimum occupancy for payout

;; Data variables
(define-data-var next-property-id uint u1)
(define-data-var next-investment-id uint u1)
(define-data-var property-manager principal tx-sender)
(define-data-var total-management-fees uint u0)

;; Property data structure
(define-map properties
    uint
    {
        developer: principal,
        property-address: (string-utf8 100),
        property-details: (string-utf8 500),
        property-type: (string-utf8 20),
        location-type: (string-utf8 10),
        investment-amount: uint,
        security-deposit: uint,
        lease-term: uint,
        is-active: bool,
        total-investments: uint,
        total-payouts: uint,
        created-at: uint
    })

;; Investment data structure
(define-map investments
    uint
    {
        investor: principal,
        property-id: uint,
        invested-at: uint,
        payout-at: uint,
        occupancy-rate: uint,
        is-paid: bool,
        is-verified: bool,
        deposit-locked: uint
    })

;; Investor investments by property
(define-map investor-property-investments
    { investor: principal, property-id: uint }
    uint)

;; Payout records
(define-map payouts
    { investor: principal, property-id: uint }
    {
        paid-at: uint,
        final-occupancy: uint,
        rental-receipt: (string-utf8 64)
    })

;; Private validation functions
(define-private (validate-property-type (property-type (string-utf8 20)))
    (or 
        (is-eq property-type u"Residential")
        (is-eq property-type u"Commercial")
        (is-eq property-type u"Industrial")
        (is-eq property-type u"Retail")
        (is-eq property-type u"Office")
        (is-eq property-type u"Warehouse")
        (is-eq property-type u"Mixed-Use")
        (is-eq property-type u"Land")
    ))

(define-private (validate-location-type (location-type (string-utf8 10)))
    (or 
        (is-eq location-type u"Urban")
        (is-eq location-type u"Suburban")
        (is-eq location-type u"Rural")
        (is-eq location-type u"Downtown")
    ))

(define-private (validate-text-length (text (string-utf8 500)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    ))

(define-private (calculate-management-fee (amount uint))
    (/ (* amount MANAGEMENT-FEE-PERCENT) u100))

(define-private (calculate-developer-amount (amount uint))
    (- amount (calculate-management-fee amount)))

(define-private (validate-deposit-amount (deposit-amount uint))
    (and (>= deposit-amount u0) (<= deposit-amount u100000000000))) ;; Max 100K STX deposit

(define-private (validate-rental-receipt (rental-receipt (string-utf8 64)))
    (and (>= (len rental-receipt) u32) (<= (len rental-receipt) u64)))

;; Public functions

;; Create a new property investment opportunity
(define-public (create-property 
    (property-address (string-utf8 100))
    (property-details (string-utf8 500))
    (property-type (string-utf8 20))
    (location-type (string-utf8 10))
    (investment-amount uint)
    (security-deposit uint)
    (lease-term uint))
    (let
        (
            (property-id (var-get next-property-id))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        ;; Validate inputs
        (asserts! (validate-text-length property-address u10 u100) ERR-INVALID-ADDRESS)
        (asserts! (validate-text-length property-details u20 u500) ERR-INVALID-DETAILS)
        (asserts! (validate-property-type property-type) ERR-INVALID-PROPERTY-TYPE)
        (asserts! (validate-location-type location-type) ERR-INVALID-LOCATION-TYPE)
        (asserts! (and (>= investment-amount MIN-INVESTMENT) (<= investment-amount MAX-INVESTMENT)) ERR-INVALID-INVESTMENT)
        (asserts! (and (>= lease-term MIN-LEASE-TERM) (<= lease-term MAX-LEASE-TERM)) ERR-INVALID-LEASE-TERM)
        (asserts! (validate-deposit-amount security-deposit) ERR-INVALID-INVESTMENT)
        
        ;; Create property
        (map-set properties property-id {
            developer: tx-sender,
            property-address: property-address,
            property-details: property-details,
            property-type: property-type,
            location-type: location-type,
            investment-amount: investment-amount,
            security-deposit: security-deposit,
            lease-term: lease-term,
            is-active: true,
            total-investments: u0,
            total-payouts: u0,
            created-at: current-time
        })
        
        (var-set next-property-id (+ property-id u1))
        (ok property-id)
    ))

;; Invest in property with security deposit
(define-public (invest-in-property (property-id uint))
    (let
        (
            (property (unwrap! (map-get? properties property-id) ERR-PROPERTY-NOT-FOUND))
            (investment-id (var-get next-investment-id))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (payout-at (+ current-time (get lease-term property)))
            (total-cost (+ (get investment-amount property) (get security-deposit property)))
            (management-fee (calculate-management-fee (get investment-amount property)))
            (developer-amount (calculate-developer-amount (get investment-amount property)))
        )
        ;; Validate property is active
        (asserts! (get is-active property) ERR-PROPERTY-INACTIVE)
        
        ;; Check if already invested
        (asserts! (is-none (map-get? investor-property-investments { investor: tx-sender, property-id: property-id })) ERR-ALREADY-INVESTED)
        
        ;; Transfer investment to developer and management fee
        (try! (stx-transfer? developer-amount tx-sender (get developer property)))
        (try! (stx-transfer? management-fee tx-sender (var-get property-manager)))
        
        ;; Lock security deposit (simulated by requiring balance)
        (asserts! (>= (stx-get-balance tx-sender) (get security-deposit property)) ERR-INSUFFICIENT-FUNDS)
        
        ;; Create investment
        (map-set investments investment-id {
            investor: tx-sender,
            property-id: property-id,
            invested-at: current-time,
            payout-at: payout-at,
            occupancy-rate: u0,
            is-paid: false,
            is-verified: false,
            deposit-locked: (get security-deposit property)
        })
        
        ;; Map investor to investment
        (map-set investor-property-investments { investor: tx-sender, property-id: property-id } investment-id)
        
        ;; Update property stats
        (map-set properties property-id (merge property { total-investments: (+ (get total-investments property) u1) }))
        
        ;; Update management fees
        (var-set total-management-fees (+ (var-get total-management-fees) management-fee))
        (var-set next-investment-id (+ investment-id u1))
        
        (ok investment-id)
    ))

;; Update occupancy rate
(define-public (update-occupancy-rate (property-id uint) (occupancy-rate uint))
    (let
        (
            (investment-id (unwrap! (map-get? investor-property-investments { investor: tx-sender, property-id: property-id }) ERR-NOT-INVESTED))
            (investment (unwrap! (map-get? investments investment-id) ERR-NOT-INVESTED))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        ;; Validate investment is active
        (asserts! (< current-time (get payout-at investment)) ERR-LEASE-ACTIVE)
        (asserts! (<= occupancy-rate u100) ERR-INVALID-AMOUNT)
        (asserts! (>= occupancy-rate (get occupancy-rate investment)) ERR-INVALID-AMOUNT)
        
        ;; Update occupancy rate
        (map-set investments investment-id (merge investment { 
            occupancy-rate: occupancy-rate,
            is-verified: (>= occupancy-rate u100)
        }))
        
        (ok true)
    ))

;; Process rental payout
(define-public (process-payout (property-id uint) (rental-receipt (string-utf8 64)))
    (let
        (
            (investment-id (unwrap! (map-get? investor-property-investments { investor: tx-sender, property-id: property-id }) ERR-NOT-INVESTED))
            (investment (unwrap! (map-get? investments investment-id) ERR-NOT-INVESTED))
            (property (unwrap! (map-get? properties property-id) ERR-PROPERTY-NOT-FOUND))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (validated-property-id (get property-id investment))
            (validated-receipt rental-receipt)
        )
        ;; Additional validations
        (asserts! (validate-rental-receipt rental-receipt) ERR-INVALID-DETAILS)
        (asserts! (is-eq property-id validated-property-id) ERR-PROPERTY-NOT-FOUND)
        
        ;; Validate occupancy and payout readiness
        (asserts! (get is-verified investment) ERR-PAYOUT-NOT-READY)
        (asserts! (>= (get occupancy-rate investment) PAYOUT-THRESHOLD) ERR-PAYOUT-NOT-READY)
        (asserts! (not (get is-paid investment)) ERR-ALREADY-PAID)
        
        ;; Process payout record
        (map-set payouts { investor: tx-sender, property-id: validated-property-id } {
            paid-at: current-time,
            final-occupancy: (get occupancy-rate investment),
            rental-receipt: validated-receipt
        })
        
        ;; Update investment
        (map-set investments investment-id (merge investment { is-paid: true }))
        
        ;; Update property stats
        (map-set properties validated-property-id (merge property { total-payouts: (+ (get total-payouts property) u1) }))
        
        ;; Return security deposit to investor (simulated)
        (ok true)
    ))

;; Deactivate property (developer only)
(define-public (deactivate-property (property-id uint))
    (let
        (
            (property (unwrap! (map-get? properties property-id) ERR-PROPERTY-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get developer property)) ERR-NOT-AUTHORIZED)
        (map-set properties property-id (merge property { is-active: false }))
        (ok true)
    ))

;; Read-only functions
(define-read-only (get-property (property-id uint))
    (map-get? properties property-id))

(define-read-only (get-investment (investment-id uint))
    (map-get? investments investment-id))

(define-read-only (get-investor-investment (investor principal) (property-id uint))
    (match (map-get? investor-property-investments { investor: investor, property-id: property-id })
        investment-id (map-get? investments investment-id)
        none
    ))

(define-read-only (get-payout (investor principal) (property-id uint))
    (map-get? payouts { investor: investor, property-id: property-id }))

(define-read-only (is-investor-paid (investor principal) (property-id uint))
    (is-some (map-get? payouts { investor: investor, property-id: property-id })))

(define-read-only (get-property-stats (property-id uint))
    (match (map-get? properties property-id)
        property {
            total-investments: (get total-investments property),
            total-payouts: (get total-payouts property),
            payout-rate: (if (> (get total-investments property) u0)
                (/ (* (get total-payouts property) u100) (get total-investments property))
                u0
            )
        }
        { total-investments: u0, total-payouts: u0, payout-rate: u0 }
    ))

(define-read-only (get-management-stats)
    {
        total-properties: (- (var-get next-property-id) u1),
        total-investments: (- (var-get next-investment-id) u1),
        total-management-fees: (var-get total-management-fees),
        property-manager: (var-get property-manager)
    })

(define-read-only (calculate-investment-cost (property-id uint))
    (match (map-get? properties property-id)
        property {
            investment: (get investment-amount property),
            deposit: (get security-deposit property),
            total: (+ (get investment-amount property) (get security-deposit property)),
            management-fee: (calculate-management-fee (get investment-amount property)),
            developer-amount: (calculate-developer-amount (get investment-amount property))
        }
        { investment: u0, deposit: u0, total: u0, management-fee: u0, developer-amount: u0 }
    ))