;; Certificate Renewal & Expiration Management System
;; Manages certificate expiration dates, renewal requirements, and continuing education tracking

(define-constant CONTRACT_OWNER tx-sender)
(define-constant CERTIFY_CONTRACT 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.Certify)

;; Error constants (400-420 range to avoid conflicts)
(define-constant ERR_NOT_AUTHORIZED (err u400))
(define-constant ERR_CERTIFICATE_NOT_FOUND (err u401))
(define-constant ERR_RENEWAL_NOT_FOUND (err u402))
(define-constant ERR_CERTIFICATE_NOT_EXPIRED (err u403))
(define-constant ERR_RENEWAL_REQUIREMENTS_NOT_MET (err u404))
(define-constant ERR_CE_ACTIVITY_NOT_FOUND (err u405))
(define-constant ERR_INVALID_EXPIRATION_DATE (err u406))
(define-constant ERR_ALREADY_EXPIRED (err u407))
(define-constant ERR_INSUFFICIENT_CE_HOURS (err u408))
(define-constant ERR_RENEWAL_PERIOD_EXPIRED (err u409))

;; Data variables
(define-data-var renewal-counter uint u0)
(define-data-var ce-activity-counter uint u0)
(define-data-var blocks-per-year uint u52560) ;; Approximate blocks in a year

;; Certificate expiration tracking
(define-map certificate-expiration
  uint ;; certificate-id
  {
    expiration-date: uint, ;; block height when cert expires
    renewal-period-months: uint, ;; how long before expiry renewal is allowed
    ce-hours-required: uint, ;; continuing education hours needed for renewal
    auto-revoke-on-expiry: bool, ;; whether to auto-revoke when expired
    grace-period-days: uint, ;; days after expiry to allow renewal
    last-renewed: (optional uint)
  }
)

;; Continuing Education activities
(define-map ce-activities
  uint ;; activity-id
  {
    certificate-id: uint,
    activity-type: (string-ascii 50), ;; "course", "conference", "workshop", "exam"
    activity-name: (string-ascii 100),
    provider: (string-ascii 100),
    hours-earned: uint,
    completion-date: uint,
    verified: bool,
    submitted-by: principal,
    evidence-hash: (optional (buff 32))
  }
)

;; Track CE hours per certificate
(define-map certificate-ce-hours
  uint ;; certificate-id
  {
    total-hours: uint,
    hours-this-period: uint,
    last-reset: uint ;; when CE hours were last reset for renewal
  }
)

;; Renewal requests and processing
(define-map renewal-requests
  uint ;; renewal-id
  {
    certificate-id: uint,
    requester: principal,
    requested-at: uint,
    new-expiration-date: uint,
    ce-hours-submitted: uint,
    status: (string-ascii 20), ;; "pending", "approved", "denied", "expired"
    processed-by: (optional principal),
    processed-at: (optional uint),
    notes: (optional (string-ascii 200))
  }
)

;; Expiration alerts and notifications
(define-map expiration-alerts
  {certificate-id: uint, alert-type: (string-ascii 20)}
  {
    alert-sent: bool,
    alert-date: uint,
    days-before-expiry: uint
  }
)

;; Set expiration parameters for a certificate (institution admin only)
(define-public (set-certificate-expiration
  (certificate-id uint)
  (expiration-date uint)
  (renewal-period-months uint)
  (ce-hours-required uint)
  (auto-revoke-on-expiry bool)
  (grace-period-days uint)
)
  (let (
    (certificate (unwrap! (contract-call? .Certify get-certificate certificate-id) ERR_CERTIFICATE_NOT_FOUND))
    (institution (unwrap! (contract-call? .Certify get-institution (get institution-id certificate)) ERR_NOT_AUTHORIZED))
  )
    (asserts! (or 
      (is-eq tx-sender (get admin institution))
      (is-eq tx-sender CONTRACT_OWNER)
    ) ERR_NOT_AUTHORIZED)
    (asserts! (> expiration-date stacks-block-height) ERR_INVALID_EXPIRATION_DATE)
    
    (map-set certificate-expiration certificate-id {
      expiration-date: expiration-date,
      renewal-period-months: renewal-period-months,
      ce-hours-required: ce-hours-required,
      auto-revoke-on-expiry: auto-revoke-on-expiry,
      grace-period-days: grace-period-days,
      last-renewed: none
    })
    
    ;; Initialize CE hours tracking
    (map-set certificate-ce-hours certificate-id {
      total-hours: u0,
      hours-this-period: u0,
      last-reset: stacks-block-height
    })
    
    (ok true)
  )
)

;; Submit continuing education activity
(define-public (submit-ce-activity
  (certificate-id uint)
  (activity-type (string-ascii 50))
  (activity-name (string-ascii 100))
  (provider (string-ascii 100))
  (hours-earned uint)
  (completion-date uint)
  (evidence-hash (optional (buff 32)))
)
  (let (
    (certificate (unwrap! (contract-call? .Certify get-certificate certificate-id) ERR_CERTIFICATE_NOT_FOUND))
    (activity-id (+ (var-get ce-activity-counter) u1))
    (ce-hours (default-to {total-hours: u0, hours-this-period: u0, last-reset: stacks-block-height} 
                (map-get? certificate-ce-hours certificate-id)))
  )
    (asserts! (is-eq tx-sender (get recipient certificate)) ERR_NOT_AUTHORIZED)
    (asserts! (> hours-earned u0) ERR_NOT_AUTHORIZED)
    
    ;; Record the CE activity
    (map-set ce-activities activity-id {
      certificate-id: certificate-id,
      activity-type: activity-type,
      activity-name: activity-name,
      provider: provider,
      hours-earned: hours-earned,
      completion-date: completion-date,
      verified: false,
      submitted-by: tx-sender,
      evidence-hash: evidence-hash
    })
    
    ;; Update CE hours (pending verification)
    (map-set certificate-ce-hours certificate-id (merge ce-hours {
      total-hours: (+ (get total-hours ce-hours) hours-earned),
      hours-this-period: (+ (get hours-this-period ce-hours) hours-earned)
    }))
    
    (var-set ce-activity-counter activity-id)
    (ok activity-id)
  )
)

;; Verify CE activity (institution admin only)
(define-public (verify-ce-activity (activity-id uint) (verified bool))
  (let (
    (activity (unwrap! (map-get? ce-activities activity-id) ERR_CE_ACTIVITY_NOT_FOUND))
    (certificate (unwrap! (contract-call? .Certify get-certificate (get certificate-id activity)) ERR_CERTIFICATE_NOT_FOUND))
    (institution (unwrap! (contract-call? .Certify get-institution (get institution-id certificate)) ERR_NOT_AUTHORIZED))
    (ce-hours (default-to {total-hours: u0, hours-this-period: u0, last-reset: stacks-block-height} 
                (map-get? certificate-ce-hours (get certificate-id activity))))
  )
    (asserts! (or 
      (is-eq tx-sender (get admin institution))
      (is-eq tx-sender CONTRACT_OWNER)
    ) ERR_NOT_AUTHORIZED)
    
    ;; If denying verification, subtract hours
    (if (and (get verified activity) (not verified))
      (map-set certificate-ce-hours (get certificate-id activity) (merge ce-hours {
        total-hours: (- (get total-hours ce-hours) (get hours-earned activity)),
        hours-this-period: (- (get hours-this-period ce-hours) (get hours-earned activity))
      }))
      true
    )
    
    (map-set ce-activities activity-id (merge activity {verified: verified}))
    (ok true)
  )
)

;; Submit renewal request
(define-public (request-renewal (certificate-id uint))
  (let (
    (certificate (unwrap! (contract-call? .Certify get-certificate certificate-id) ERR_CERTIFICATE_NOT_FOUND))
    (expiration-info (unwrap! (map-get? certificate-expiration certificate-id) ERR_CERTIFICATE_NOT_FOUND))
    (ce-hours (default-to {total-hours: u0, hours-this-period: u0, last-reset: stacks-block-height} 
                (map-get? certificate-ce-hours certificate-id)))
    (renewal-id (+ (var-get renewal-counter) u1))
    (renewal-period-blocks (* (get renewal-period-months expiration-info) (/ (var-get blocks-per-year) u12)))
    (renewal-start (- (get expiration-date expiration-info) renewal-period-blocks))
  )
    (asserts! (is-eq tx-sender (get recipient certificate)) ERR_NOT_AUTHORIZED)
    (asserts! (>= stacks-block-height renewal-start) ERR_CERTIFICATE_NOT_EXPIRED)
    (asserts! (>= (get hours-this-period ce-hours) (get ce-hours-required expiration-info)) ERR_INSUFFICIENT_CE_HOURS)
    
    (let ((new-expiration (+ (get expiration-date expiration-info) (var-get blocks-per-year))))
      (map-set renewal-requests renewal-id {
        certificate-id: certificate-id,
        requester: tx-sender,
        requested-at: stacks-block-height,
        new-expiration-date: new-expiration,
        ce-hours-submitted: (get hours-this-period ce-hours),
        status: "pending",
        processed-by: none,
        processed-at: none,
        notes: none
      })
    )
    
    (var-set renewal-counter renewal-id)
    (ok renewal-id)
  )
)

;; Process renewal request (institution admin only)
(define-public (process-renewal-request 
  (renewal-id uint) 
  (approved bool)
  (notes (optional (string-ascii 200)))
)
  (let (
    (renewal-request (unwrap! (map-get? renewal-requests renewal-id) ERR_RENEWAL_NOT_FOUND))
    (certificate (unwrap! (contract-call? .Certify get-certificate (get certificate-id renewal-request)) ERR_CERTIFICATE_NOT_FOUND))
    (institution (unwrap! (contract-call? .Certify get-institution (get institution-id certificate)) ERR_NOT_AUTHORIZED))
    (expiration-info (unwrap! (map-get? certificate-expiration (get certificate-id renewal-request)) ERR_CERTIFICATE_NOT_FOUND))
  )
    (asserts! (or 
      (is-eq tx-sender (get admin institution))
      (is-eq tx-sender CONTRACT_OWNER)
    ) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status renewal-request) "pending") ERR_NOT_AUTHORIZED)
    
    ;; Update renewal request status
    (map-set renewal-requests renewal-id (merge renewal-request {
      status: (if approved "approved" "denied"),
      processed-by: (some tx-sender),
      processed-at: (some stacks-block-height),
      notes: notes
    }))
    
    ;; If approved, update certificate expiration and reset CE hours
    (if approved
      (begin
        (map-set certificate-expiration (get certificate-id renewal-request) (merge expiration-info {
          expiration-date: (get new-expiration-date renewal-request),
          last-renewed: (some stacks-block-height)
        }))
        (map-set certificate-ce-hours (get certificate-id renewal-request) {
          total-hours: (get ce-hours-submitted renewal-request),
          hours-this-period: u0,
          last-reset: stacks-block-height
        })
      )
      true
    )
    
    (ok approved)
  )
)

;; Check if certificate is expired or expiring soon
(define-public (check-certificate-expiration (certificate-id uint))
  (let (
    (expiration-info (unwrap! (map-get? certificate-expiration certificate-id) ERR_CERTIFICATE_NOT_FOUND))
    (blocks-until-expiry (- (get expiration-date expiration-info) stacks-block-height))
    (days-until-expiry (/ blocks-until-expiry u144)) ;; approximate days
  )
    (if (<= blocks-until-expiry u0)
      ;; Certificate is expired
      (begin
        (if (get auto-revoke-on-expiry expiration-info)
          (unwrap-panic (contract-call? .Certify revoke-certificate certificate-id))
          true
        )
        (ok {status: "expired", days-until-expiry: u0, blocks-until-expiry: u0})
      )
      ;; Certificate is still valid
      (ok {
        status: (if (<= days-until-expiry u30) "expiring-soon" "valid"),
        days-until-expiry: days-until-expiry,
        blocks-until-expiry: blocks-until-expiry
      })
    )
  )
)

;; Read-only functions
(define-read-only (get-certificate-expiration-info (certificate-id uint))
  (map-get? certificate-expiration certificate-id)
)

(define-read-only (get-certificate-ce-hours-info (certificate-id uint))
  (map-get? certificate-ce-hours certificate-id)
)

(define-read-only (get-ce-activity (activity-id uint))
  (map-get? ce-activities activity-id)
)

(define-read-only (get-renewal-request (renewal-id uint))
  (map-get? renewal-requests renewal-id)
)

(define-read-only (is-certificate-expired (certificate-id uint))
  (match (map-get? certificate-expiration certificate-id)
    expiration-info (< (get expiration-date expiration-info) stacks-block-height)
    false
  )
)

(define-read-only (get-renewal-eligibility (certificate-id uint))
  (match (map-get? certificate-expiration certificate-id)
    expiration-info (let (
      (renewal-period-blocks (* (get renewal-period-months expiration-info) (/ (var-get blocks-per-year) u12)))
      (renewal-start (- (get expiration-date expiration-info) renewal-period-blocks))
      (ce-hours (default-to {total-hours: u0, hours-this-period: u0, last-reset: stacks-block-height} 
                  (map-get? certificate-ce-hours certificate-id)))
    )
      {
        eligible: (and 
          (>= stacks-block-height renewal-start)
          (>= (get hours-this-period ce-hours) (get ce-hours-required expiration-info))
        ),
        ce-hours-needed: (get ce-hours-required expiration-info),
        ce-hours-earned: (get hours-this-period ce-hours),
        renewal-window-open: (>= stacks-block-height renewal-start)
      }
    )
    {eligible: false, ce-hours-needed: u0, ce-hours-earned: u0, renewal-window-open: false}
  )
)
