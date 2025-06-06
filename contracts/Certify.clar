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