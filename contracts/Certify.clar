(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_CERTIFICATE_NOT_FOUND (err u101))
(define-constant ERR_CERTIFICATE_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_INSTITUTION (err u103))
(define-constant ERR_CERTIFICATE_REVOKED (err u104))

(define-data-var certificate-counter uint u0)

(define-map institutions
  { institution-id: uint }
  {
    name: (string-ascii 100),
    authorized: bool,
    admin: principal
  }
)

(define-map institution-counter
  { dummy: bool }
  { counter: uint }
)

(define-map certificates
  { certificate-id: uint }
  {
    recipient: principal,
    institution-id: uint,
    degree-type: (string-ascii 50),
    field-of-study: (string-ascii 100),
    graduation-date: uint,
    gpa: (optional uint),
    honors: (optional (string-ascii 50)),
    issued-at: uint,
    revoked: bool
  }
)

(define-map recipient-certificates
  { recipient: principal, index: uint }
  { certificate-id: uint }
)

(define-map recipient-certificate-count
  { recipient: principal }
  { count: uint }
)

(define-map institution-certificates
  { institution-id: uint, index: uint }
  { certificate-id: uint }
)

(define-map institution-certificate-count
  { institution-id: uint }
  { count: uint }
)

(define-public (register-institution (name (string-ascii 100)) (admin principal))
  (let
    (
      (current-counter (default-to u0 (get counter (map-get? institution-counter { dummy: true }))))
      (new-institution-id (+ current-counter u1))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set institutions
      { institution-id: new-institution-id }
      {
        name: name,
        authorized: true,
        admin: admin
      }
    )
    (map-set institution-counter { dummy: true } { counter: new-institution-id })
    (ok new-institution-id)
  )
)

(define-public (issue-certificate 
  (recipient principal)
  (institution-id uint)
  (degree-type (string-ascii 50))
  (field-of-study (string-ascii 100))
  (graduation-date uint)
  (gpa (optional uint))
  (honors (optional (string-ascii 50)))
)
  (let
    (
      (institution (unwrap! (map-get? institutions { institution-id: institution-id }) ERR_INVALID_INSTITUTION))
      (current-counter (var-get certificate-counter))
      (new-certificate-id (+ current-counter u1))
      (recipient-count (default-to u0 (get count (map-get? recipient-certificate-count { recipient: recipient }))))
      (institution-count (default-to u0 (get count (map-get? institution-certificate-count { institution-id: institution-id }))))
    )
    (asserts! (get authorized institution) ERR_INVALID_INSTITUTION)
    (asserts! (or (is-eq tx-sender (get admin institution)) (is-eq tx-sender CONTRACT_OWNER)) ERR_NOT_AUTHORIZED)
    
    (map-set certificates
      { certificate-id: new-certificate-id }
      {
        recipient: recipient,
        institution-id: institution-id,
        degree-type: degree-type,
        field-of-study: field-of-study,
        graduation-date: graduation-date,
        gpa: gpa,
        honors: honors,
        issued-at: stacks-block-height,
        revoked: false
      }
    )
    
    (map-set recipient-certificates
      { recipient: recipient, index: recipient-count }
      { certificate-id: new-certificate-id }
    )
    
    (map-set recipient-certificate-count
      { recipient: recipient }
      { count: (+ recipient-count u1) }
    )
    
    (map-set institution-certificates
      { institution-id: institution-id, index: institution-count }
      { certificate-id: new-certificate-id }
    )
    
    (map-set institution-certificate-count
      { institution-id: institution-id }
      { count: (+ institution-count u1) }
    )
    
    (var-set certificate-counter new-certificate-id)
    (ok new-certificate-id)
  )
)

(define-public (revoke-certificate (certificate-id uint))
  (let
    (
      (certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_CERTIFICATE_NOT_FOUND))
      (institution (unwrap! (map-get? institutions { institution-id: (get institution-id certificate) }) ERR_INVALID_INSTITUTION))
    )
    (asserts! (or (is-eq tx-sender (get admin institution)) (is-eq tx-sender CONTRACT_OWNER)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get revoked certificate)) ERR_CERTIFICATE_REVOKED)
    
    (map-set certificates
      { certificate-id: certificate-id }
      (merge certificate { revoked: true })
    )
    (ok true)
  )
)

(define-public (update-institution-authorization (institution-id uint) (authorized bool))
  (let
    (
      (institution (unwrap! (map-get? institutions { institution-id: institution-id }) ERR_INVALID_INSTITUTION))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set institutions
      { institution-id: institution-id }
      (merge institution { authorized: authorized })
    )
    (ok true)
  )
)

(define-read-only (get-certificate (certificate-id uint))
  (map-get? certificates { certificate-id: certificate-id })
)

(define-read-only (get-institution (institution-id uint))
  (map-get? institutions { institution-id: institution-id })
)

(define-read-only (verify-certificate (certificate-id uint))
  (match (map-get? certificates { certificate-id: certificate-id })
    certificate (ok {
      valid: (not (get revoked certificate)),
      recipient: (get recipient certificate),
      institution-id: (get institution-id certificate),
      degree-type: (get degree-type certificate),
      field-of-study: (get field-of-study certificate),
      graduation-date: (get graduation-date certificate),
      issued-at: (get issued-at certificate)
    })
    ERR_CERTIFICATE_NOT_FOUND
  )
)

(define-read-only (get-recipient-certificate-count (recipient principal))
  (default-to u0 (get count (map-get? recipient-certificate-count { recipient: recipient })))
)

(define-read-only (get-recipient-certificate-by-index (recipient principal) (index uint))
  (match (map-get? recipient-certificates { recipient: recipient, index: index })
    cert-ref (map-get? certificates { certificate-id: (get certificate-id cert-ref) })
    none
  )
)

(define-read-only (get-institution-certificate-count (institution-id uint))
  (default-to u0 (get count (map-get? institution-certificate-count { institution-id: institution-id })))
)

(define-read-only (get-institution-certificate-by-index (institution-id uint) (index uint))
  (match (map-get? institution-certificates { institution-id: institution-id, index: index })
    cert-ref (map-get? certificates { certificate-id: (get certificate-id cert-ref) })
    none
  )
)

(define-read-only (get-total-certificates)
  (var-get certificate-counter)
)

(define-read-only (get-total-institutions)
  (default-to u0 (get counter (map-get? institution-counter { dummy: true })))
)

(define-constant ERR_SKILL_NOT_FOUND (err u105))
(define-constant ERR_ENDORSEMENT_NOT_FOUND (err u106))
(define-constant ERR_SKILL_ALREADY_EXISTS (err u107))
(define-constant ERR_ALREADY_ENDORSED (err u108))
(define-constant ERR_CANNOT_ENDORSE_OWN_SKILL (err u109))
(define-constant ERR_INVALID_SKILL_LEVEL (err u110))

(define-data-var skill-counter uint u0)
(define-data-var endorsement-counter uint u0)

(define-map certificate-skills
  { certificate-id: uint, skill-index: uint }
  {
    skill-id: uint,
    skill-name: (string-ascii 50),
    skill-category: (string-ascii 30),
    proficiency-level: uint,
    added-by: principal,
    added-at: uint
  }
)

(define-map certificate-skill-count
  { certificate-id: uint }
  { count: uint }
)

(define-map skill-registry
  { skill-id: uint }
  {
    name: (string-ascii 50),
    category: (string-ascii 30),
    description: (string-ascii 200),
    created-by: principal,
    created-at: uint,
    total-endorsements: uint
  }
)

(define-map skill-endorsements
  { skill-id: uint, endorsement-index: uint }
  {
    endorsement-id: uint,
    certificate-id: uint,
    endorser: principal,
    endorser-type: (string-ascii 20),
    endorsement-weight: uint,
    comment: (optional (string-ascii 150)),
    endorsed-at: uint
  }
)

(define-map skill-endorsement-count
  { skill-id: uint }
  { count: uint }
)

(define-map endorser-credentials
  { endorser: principal }
  {
    endorser-type: (string-ascii 20),
    institution-id: (optional uint),
    verified: bool,
    reputation-score: uint
  }
)

(define-map recipient-skill-summary
  { recipient: principal, skill-name: (string-ascii 50) }
  {
    total-endorsements: uint,
    average-weight: uint,
    highest-proficiency: uint,
    institutions-count: uint
  }
)

(define-public (register-skill 
  (name (string-ascii 50))
  (category (string-ascii 30))
  (description (string-ascii 200))
)
  (let
    (
      (current-counter (var-get skill-counter))
      (new-skill-id (+ current-counter u1))
    )
    (asserts! (is-none (map-get? skill-registry { skill-id: new-skill-id })) ERR_SKILL_ALREADY_EXISTS)
    
    (map-set skill-registry
      { skill-id: new-skill-id }
      {
        name: name,
        category: category,
        description: description,
        created-by: tx-sender,
        created-at: stacks-block-height,
        total-endorsements: u0
      }
    )
    
    (var-set skill-counter new-skill-id)
    (ok new-skill-id)
  )
)

(define-public (add-certificate-skill
  (certificate-id uint)
  (skill-name (string-ascii 50))
  (skill-category (string-ascii 30))
  (proficiency-level uint)
)
  (let
    (
      (certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_CERTIFICATE_NOT_FOUND))
      (institution (unwrap! (map-get? institutions { institution-id: (get institution-id certificate) }) ERR_INVALID_INSTITUTION))
      (current-skill-count (default-to u0 (get count (map-get? certificate-skill-count { certificate-id: certificate-id }))))
      (current-counter (var-get skill-counter))
      (new-skill-id (+ current-counter u1))
    )
    (asserts! (> proficiency-level u0) ERR_INVALID_SKILL_LEVEL)
    (asserts! (<= proficiency-level u5) ERR_INVALID_SKILL_LEVEL)
    (asserts! (or 
      (is-eq tx-sender (get admin institution))
      (is-eq tx-sender CONTRACT_OWNER)
      (is-eq tx-sender (get recipient certificate))
    ) ERR_NOT_AUTHORIZED)
    
    (map-set certificate-skills
      { certificate-id: certificate-id, skill-index: current-skill-count }
      {
        skill-id: new-skill-id,
        skill-name: skill-name,
        skill-category: skill-category,
        proficiency-level: proficiency-level,
        added-by: tx-sender,
        added-at: stacks-block-height
      }
    )
    
    (map-set certificate-skill-count
      { certificate-id: certificate-id }
      { count: (+ current-skill-count u1) }
    )
    
    (var-set skill-counter new-skill-id)
    (ok new-skill-id)
  )
)

(define-public (register-endorser
  (endorser-type (string-ascii 20))
  (institution-id (optional uint))
)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    (map-set endorser-credentials
      { endorser: tx-sender }
      {
        endorser-type: endorser-type,
        institution-id: institution-id,
        verified: true,
        reputation-score: u100
      }
    )
    (ok true)
  )
)

(define-public (endorse-skill
  (certificate-id uint)
  (skill-name (string-ascii 50))
  (endorser-type (string-ascii 20))
  (endorsement-weight uint)
  (comment (optional (string-ascii 150)))
)
  (let
    (
      (certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_CERTIFICATE_NOT_FOUND))
      (skill-found (get-certificate-skill-by-name certificate-id skill-name))
      (current-endorsement-count (var-get endorsement-counter))
      (new-endorsement-id (+ current-endorsement-count u1))
      (skill-endorsement-count-val (default-to u0 (get count (map-get? skill-endorsement-count { skill-id: u1 }))))
    )
    (asserts! (is-some skill-found) ERR_SKILL_NOT_FOUND)
    (asserts! (not (is-eq tx-sender (get recipient certificate))) ERR_CANNOT_ENDORSE_OWN_SKILL)
    (asserts! (> endorsement-weight u0) ERR_INVALID_SKILL_LEVEL)
    (asserts! (<= endorsement-weight u10) ERR_INVALID_SKILL_LEVEL)
    
    (map-set skill-endorsements
      { skill-id: u1, endorsement-index: skill-endorsement-count-val }
      {
        endorsement-id: new-endorsement-id,
        certificate-id: certificate-id,
        endorser: tx-sender,
        endorser-type: endorser-type,
        endorsement-weight: endorsement-weight,
        comment: comment,
        endorsed-at: stacks-block-height
      }
    )
    
    (map-set skill-endorsement-count
      { skill-id: u1 }
      { count: (+ skill-endorsement-count-val u1) }
    )
    
    (var-set endorsement-counter new-endorsement-id)
    (unwrap-panic (update-recipient-skill-summary (get recipient certificate) skill-name endorsement-weight))
    (ok new-endorsement-id)
  )
)

(define-private (update-recipient-skill-summary
  (recipient principal)
  (skill-name (string-ascii 50))
  (new-endorsement-weight uint)
)
  (let
    (
      (current-summary (map-get? recipient-skill-summary { recipient: recipient, skill-name: skill-name }))
      (total-endorsements (+ (default-to u0 (get total-endorsements current-summary)) u1))
      (current-avg (default-to u0 (get average-weight current-summary)))
      (new-average (/ (+ (* current-avg (- total-endorsements u1)) new-endorsement-weight) total-endorsements))
    )
    
    (map-set recipient-skill-summary
      { recipient: recipient, skill-name: skill-name }
      {
        total-endorsements: total-endorsements,
        average-weight: new-average,
        highest-proficiency: (if (> new-endorsement-weight (default-to u0 (get highest-proficiency current-summary))) new-endorsement-weight (default-to u0 (get highest-proficiency current-summary))),
        institutions-count: (+ (default-to u0 (get institutions-count current-summary)) u1)
      }
    )
    (ok true)
  )
)

(define-read-only (get-certificate-skills (certificate-id uint))
  (let
    (
      (skill-count (default-to u0 (get count (map-get? certificate-skill-count { certificate-id: certificate-id }))))
    )
    (map get-certificate-skill-by-index-helper (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9))
  )
)

(define-read-only (get-certificate-skill-by-index (certificate-id uint) (index uint))
  (map-get? certificate-skills { certificate-id: certificate-id, skill-index: index })
)

(define-read-only (get-certificate-skill-by-name (certificate-id uint) (skill-name (string-ascii 50)))
  (let
    (
      (skill-count (default-to u0 (get count (map-get? certificate-skill-count { certificate-id: certificate-id }))))
    )
    (fold check-skill-name (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9) none)
  )
)

(define-read-only (get-skill-endorsements (skill-id uint))
  (let
    (
      (endorsement-count (default-to u0 (get count (map-get? skill-endorsement-count { skill-id: skill-id }))))
    )
    (map get-skill-endorsement-by-index-helper (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9))
  )
)

(define-read-only (get-skill-endorsement-by-index (skill-id uint) (index uint))
  (map-get? skill-endorsements { skill-id: skill-id, endorsement-index: index })
)

(define-read-only (get-recipient-skill-summary-info (recipient principal) (skill-name (string-ascii 50)))
  (map-get? recipient-skill-summary { recipient: recipient, skill-name: skill-name })
)

(define-read-only (get-endorser-credentials-info (endorser principal))
  (map-get? endorser-credentials { endorser: endorser })
)

(define-read-only (get-skill-info (skill-id uint))
  (map-get? skill-registry { skill-id: skill-id })
)

(define-read-only (get-certificate-skill-count (certificate-id uint))
  (default-to u0 (get count (map-get? certificate-skill-count { certificate-id: certificate-id })))
)

(define-read-only (get-skill-endorsement-count-info (skill-id uint))
  (default-to u0 (get count (map-get? skill-endorsement-count { skill-id: skill-id })))
)

(define-private (check-skill-name (index uint) (current-result (optional {skill-id: uint, skill-name: (string-ascii 50), skill-category: (string-ascii 30), proficiency-level: uint, added-by: principal, added-at: uint})))
  current-result
)

(define-private (get-certificate-skill-by-index-helper (index uint))
  (map-get? certificate-skills { certificate-id: u1, skill-index: index })
)

(define-private (get-skill-endorsement-by-index-helper (index uint))
  (map-get? skill-endorsements { skill-id: u1, endorsement-index: index })
)

;; Certificate Verification History & Analytics System
(define-constant ERR_VERIFIER_NOT_REGISTERED (err u111))
(define-constant ERR_VERIFICATION_NOT_FOUND (err u112))
(define-constant ERR_INVALID_VERIFICATION_PURPOSE (err u113))
(define-constant ERR_VERIFICATION_ALREADY_EXISTS (err u114))

(define-data-var verification-counter uint u0)
(define-data-var verifier-counter uint u0)

;; Track individual verification events
(define-map certificate-verifications
  { certificate-id: uint, verification-index: uint }
  {
    verification-id: uint,
    verifier: principal,
    verification-purpose: (string-ascii 100), ;; "employment", "admission", "audit", etc.
    verification-date: uint,
    verification-result: bool, ;; true if valid, false if issues found
    notes: (optional (string-ascii 200)),
    verifier-reputation: uint ;; snapshot of verifier reputation at time of verification
  }
)

;; Count verifications per certificate
(define-map certificate-verification-count
  { certificate-id: uint }
  { count: uint }
)

;; Track verifier profiles and reputation
(define-map registered-verifiers
  { verifier: principal }
  {
    verifier-id: uint,
    organization: (string-ascii 100),
    verifier-type: (string-ascii 50), ;; "employer", "institution", "auditor", "government"
    registration-date: uint,
    total-verifications: uint,
    successful-verifications: uint,
    reputation-score: uint, ;; 0-1000 scale
    verified-status: bool,
    last-activity: uint
  }
)

;; Analytics data per certificate
(define-map certificate-analytics
  { certificate-id: uint }
  {
    total-verifications: uint,
    unique-verifiers: uint,
    last-verified: uint,
    verification-rate: uint, ;; verifications per month
    trust-score: uint, ;; 0-1000 based on verification patterns
    most-common-purpose: (string-ascii 100),
    suspicious-activity: bool
  }
)

;; Track verification patterns by purpose
(define-map verification-purposes
  { purpose: (string-ascii 100) }
  {
    total-count: uint,
    success-rate: uint,
    avg-verifier-reputation: uint
  }
)

;; Track verifier networks (who verifies together)
(define-map verifier-relationships
  { verifier1: principal, verifier2: principal }
  {
    shared-verifications: uint,
    relationship-strength: uint, ;; 0-100
    first-interaction: uint,
    last-interaction: uint
  }
)

;; Monthly verification statistics
(define-map monthly-verification-stats
  { year: uint, month: uint }
  {
    total-verifications: uint,
    unique-certificates: uint,
    unique-verifiers: uint,
    avg-trust-score: uint
  }
)

;; Register as a verified entity that can perform certificate verifications
(define-public (register-verifier 
  (organization (string-ascii 100))
  (verifier-type (string-ascii 50))
)
  (let
    (
      (current-counter (var-get verifier-counter))
      (new-verifier-id (+ current-counter u1))
      (existing-verifier (map-get? registered-verifiers { verifier: tx-sender }))
    )
    ;; Check if already registered
    (asserts! (is-none existing-verifier) ERR_VERIFICATION_ALREADY_EXISTS)
    
    ;; Register the verifier
    (map-set registered-verifiers
      { verifier: tx-sender }
      {
        verifier-id: new-verifier-id,
        organization: organization,
        verifier-type: verifier-type,
        registration-date: stacks-block-height,
        total-verifications: u0,
        successful-verifications: u0,
        reputation-score: u500, ;; Start with medium reputation
        verified-status: false, ;; Needs manual verification by contract owner
        last-activity: stacks-block-height
      }
    )
    
    (var-set verifier-counter new-verifier-id)
    (ok new-verifier-id)
  )
)

;; Verify a verifier's status (only contract owner)
(define-public (verify-verifier-status (verifier principal) (verified bool))
  (let
    (
      (verifier-data (unwrap! (map-get? registered-verifiers { verifier: verifier }) ERR_VERIFIER_NOT_REGISTERED))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    (map-set registered-verifiers
      { verifier: verifier }
      (merge verifier-data { verified-status: verified })
    )
    (ok true)
  )
)

;; Perform a certificate verification
(define-public (verify-certificate-detailed
  (certificate-id uint)
  (verification-purpose (string-ascii 100))
  (verification-result bool)
  (notes (optional (string-ascii 200)))
)
  (let
    (
      (certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_CERTIFICATE_NOT_FOUND))
      (verifier-data (unwrap! (map-get? registered-verifiers { verifier: tx-sender }) ERR_VERIFIER_NOT_REGISTERED))
      (current-verification-count (default-to u0 (get count (map-get? certificate-verification-count { certificate-id: certificate-id }))))
      (current-counter (var-get verification-counter))
      (new-verification-id (+ current-counter u1))
    )
    ;; Ensure verifier is verified
    (asserts! (get verified-status verifier-data) ERR_NOT_AUTHORIZED)
    ;; Ensure certificate is not revoked
    (asserts! (not (get revoked certificate)) ERR_CERTIFICATE_REVOKED)
    
    ;; Record the verification
    (map-set certificate-verifications
      { certificate-id: certificate-id, verification-index: current-verification-count }
      {
        verification-id: new-verification-id,
        verifier: tx-sender,
        verification-purpose: verification-purpose,
        verification-date: stacks-block-height,
        verification-result: verification-result,
        notes: notes,
        verifier-reputation: (get reputation-score verifier-data)
      }
    )
    
    ;; Update verification count for certificate
    (map-set certificate-verification-count
      { certificate-id: certificate-id }
      { count: (+ current-verification-count u1) }
    )
    
    ;; Update verifier statistics
    (map-set registered-verifiers
      { verifier: tx-sender }
      (merge verifier-data {
        total-verifications: (+ (get total-verifications verifier-data) u1),
        successful-verifications: (if verification-result 
          (+ (get successful-verifications verifier-data) u1)
          (get successful-verifications verifier-data)
        ),
        last-activity: stacks-block-height
      })
    )
    
    ;; Update verification purposes statistics
    (unwrap-panic (update-verification-purpose-stats verification-purpose verification-result (get reputation-score verifier-data)))
    
    ;; Update certificate analytics
    (unwrap-panic (update-certificate-analytics certificate-id verification-purpose))
    
    (var-set verification-counter new-verification-id)
    (ok new-verification-id)
  )
)

;; Private function to update verification purpose statistics
(define-private (update-verification-purpose-stats 
  (purpose (string-ascii 100))
  (result bool)
  (verifier-reputation uint)
)
  (let
    (
      (current-stats (map-get? verification-purposes { purpose: purpose }))
      (total-count (+ (default-to u0 (get total-count current-stats)) u1))
      (current-successes (default-to u0 (get success-rate current-stats)))
      (new-success-rate (if result 
        (/ (* (+ current-successes u1) u100) total-count)
        (/ (* current-successes u100) total-count)
      ))
      (current-avg-rep (default-to u500 (get avg-verifier-reputation current-stats)))
      (new-avg-rep (/ (+ (* current-avg-rep (- total-count u1)) verifier-reputation) total-count))
    )
    
    (map-set verification-purposes
      { purpose: purpose }
      {
        total-count: total-count,
        success-rate: new-success-rate,
        avg-verifier-reputation: new-avg-rep
      }
    )
    (ok true)
  )
)

;; Private function to update certificate analytics
(define-private (update-certificate-analytics 
  (certificate-id uint)
  (purpose (string-ascii 100))
)
  (let
    (
      (current-analytics (map-get? certificate-analytics { certificate-id: certificate-id }))
      (total-verifications (+ (default-to u0 (get total-verifications current-analytics)) u1))
      (current-trust-score (default-to u500 (get trust-score current-analytics)))
      ;; Simple trust score calculation based on verification count and recency
      (new-trust-score (if (> total-verifications u5) 
        (if (> (+ current-trust-score u50) u1000) u1000 (+ current-trust-score u50))
        (+ current-trust-score u20)
      ))
    )
    
    (map-set certificate-analytics
      { certificate-id: certificate-id }
      {
        total-verifications: total-verifications,
        unique-verifiers: (+ (default-to u0 (get unique-verifiers current-analytics)) u1), ;; Simplified
        last-verified: stacks-block-height,
        verification-rate: (/ total-verifications u30), ;; Simplified rate calculation
        trust-score: new-trust-score,
        most-common-purpose: purpose, ;; Simplified - would need more logic for accurate tracking
        suspicious-activity: false ;; Simplified - would need pattern analysis
      }
    )
    (ok true)
  )
)

;; Get verification history for a certificate
(define-read-only (get-certificate-verification-history (certificate-id uint))
  (let
    (
      (verification-count (default-to u0 (get count (map-get? certificate-verification-count { certificate-id: certificate-id }))))
    )
    ;; Return first 10 verifications (simplified)
    (map get-verification-by-index-helper (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9))
  )
)

;; Get specific verification by index
(define-read-only (get-certificate-verification-by-index (certificate-id uint) (index uint))
  (map-get? certificate-verifications { certificate-id: certificate-id, verification-index: index })
)

;; Get verifier profile information
(define-read-only (get-verifier-profile (verifier principal))
  (map-get? registered-verifiers { verifier: verifier })
)

;; Get certificate analytics
(define-read-only (get-certificate-analytics-info (certificate-id uint))
  (map-get? certificate-analytics { certificate-id: certificate-id })
)

;; Get verification purpose statistics
(define-read-only (get-verification-purpose-stats (purpose (string-ascii 100)))
  (map-get? verification-purposes { purpose: purpose })
)

;; Get verification count for a certificate
(define-read-only (get-certificate-verification-count-info (certificate-id uint))
  (default-to u0 (get count (map-get? certificate-verification-count { certificate-id: certificate-id })))
)

;; Calculate verifier reputation score
(define-read-only (calculate-verifier-reputation (verifier principal))
  (match (map-get? registered-verifiers { verifier: verifier })
    verifier-data (let
      (
        (total-verifs (get total-verifications verifier-data))
        (successful-verifs (get successful-verifications verifier-data))
        (base-score (get reputation-score verifier-data))
      )
      (if (> total-verifs u0)
        (+ base-score (/ (* successful-verifs u100) total-verifs))
        base-score
      )
    )
    u0
  )
)

;; Get top verifiers by reputation
(define-read-only (get-top-verifiers)
  ;; Simplified - would return list of top verifiers by reputation
  (some { message: "Top verifiers feature would require iteration logic" })
)

;; Private helper function for verification history
(define-private (get-verification-by-index-helper (index uint))
  (map-get? certificate-verifications { certificate-id: u1, verification-index: index })
)

;; Update verifier reputation (contract owner only)
(define-public (update-verifier-reputation (verifier principal) (new-reputation uint))
  (let
    (
      (verifier-data (unwrap! (map-get? registered-verifiers { verifier: verifier }) ERR_VERIFIER_NOT_REGISTERED))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-reputation u1000) ERR_INVALID_SKILL_LEVEL)
    
    (map-set registered-verifiers
      { verifier: verifier }
      (merge verifier-data { reputation-score: new-reputation })
    )
    (ok true)
  )
)


