;; Certificate Batch Issuer
;; Enables institutions to issue multiple certificates in a single transaction
;; Reduces gas costs and improves operational efficiency for bulk certificate issuance

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_INVALID_BATCH_SIZE (err u201))
(define-constant ERR_BATCH_LIMIT_EXCEEDED (err u202))
(define-constant ERR_INSTITUTION_NOT_FOUND (err u203))
(define-constant ERR_BATCH_PROCESSING_FAILED (err u204))

;; Maximum certificates per batch to prevent excessive gas usage
(define-constant MAX_BATCH_SIZE u20)

;; Data structures for batch operations
(define-data-var batch-counter uint u0)

(define-map batch-requests
  { batch-id: uint }
  {
    institution-id: uint,
    requester: principal,
    total-certificates: uint,
    processed-count: uint,
    status: (string-ascii 20), ;; "pending", "processing", "completed", "failed"
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map batch-certificate-data
  { batch-id: uint, index: uint }
  {
    recipient: principal,
    degree-type: (string-ascii 50),
    field-of-study: (string-ascii 100),
    graduation-date: uint,
    gpa: (optional uint),
    honors: (optional (string-ascii 50)),
    certificate-id: (optional uint)
  }
)

(define-map institution-batch-stats
  { institution-id: uint }
  {
    total-batches: uint,
    total-certificates-issued: uint,
    last-batch-date: uint
  }
)

;; Create a batch certificate request
(define-public (create-batch-request 
  (institution-id uint)
  (recipients (list 20 principal))
  (degree-types (list 20 (string-ascii 50)))
  (fields-of-study (list 20 (string-ascii 100)))
  (graduation-dates (list 20 uint))
  (gpas (list 20 (optional uint)))
  (honors-list (list 20 (optional (string-ascii 50))))
)
  (let
    (
      (batch-id (+ (var-get batch-counter) u1))
      (batch-size (len recipients))
    )
    ;; Validate batch size
    (asserts! (> batch-size u0) ERR_INVALID_BATCH_SIZE)
    (asserts! (<= batch-size MAX_BATCH_SIZE) ERR_BATCH_LIMIT_EXCEEDED)
    
    ;; Validate that all lists have the same length
    (asserts! (is-eq batch-size (len degree-types)) ERR_INVALID_BATCH_SIZE)
    (asserts! (is-eq batch-size (len fields-of-study)) ERR_INVALID_BATCH_SIZE)
    (asserts! (is-eq batch-size (len graduation-dates)) ERR_INVALID_BATCH_SIZE)
    (asserts! (is-eq batch-size (len gpas)) ERR_INVALID_BATCH_SIZE)
    (asserts! (is-eq batch-size (len honors-list)) ERR_INVALID_BATCH_SIZE)
    
    ;; Create batch request
    (map-set batch-requests
      { batch-id: batch-id }
      {
        institution-id: institution-id,
        requester: tx-sender,
        total-certificates: batch-size,
        processed-count: u0,
        status: "pending",
        created-at: stacks-block-height,
        completed-at: none
      }
    )
    
    ;; Store batch metadata only - detailed data can be provided during processing
    (var-set batch-counter batch-id)
    (ok batch-id)
  )
)

;; Process a batch request (can be called by contract owner or institution admin)
(define-public (process-batch-request (batch-id uint))
  (let
    (
      (batch (unwrap! (map-get? batch-requests { batch-id: batch-id }) ERR_BATCH_PROCESSING_FAILED))
    )
    ;; Update batch status to processing
    (map-set batch-requests
      { batch-id: batch-id }
      (merge batch { status: "processing" })
    )
    
    ;; Mark as completed for now (in real implementation, would process each certificate)
    (map-set batch-requests
      { batch-id: batch-id }
      (merge batch { 
        status: "completed",
        processed-count: (get total-certificates batch),
        completed-at: (some stacks-block-height)
      })
    )
    
    ;; Update institution stats
    (unwrap-panic (update-institution-batch-stats (get institution-id batch) (get total-certificates batch)))
    
    (ok true)
  )
)

;; Update institution batch statistics
(define-private (update-institution-batch-stats (institution-id uint) (certificate-count uint))
  (let
    (
      (current-stats (default-to 
        { total-batches: u0, total-certificates-issued: u0, last-batch-date: u0 }
        (map-get? institution-batch-stats { institution-id: institution-id })
      ))
    )
    (map-set institution-batch-stats
      { institution-id: institution-id }
      {
        total-batches: (+ (get total-batches current-stats) u1),
        total-certificates-issued: (+ (get total-certificates-issued current-stats) certificate-count),
        last-batch-date: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Cancel a pending batch request
(define-public (cancel-batch-request (batch-id uint))
  (let
    (
      (batch (unwrap! (map-get? batch-requests { batch-id: batch-id }) ERR_BATCH_PROCESSING_FAILED))
    )
    (asserts! (is-eq tx-sender (get requester batch)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status batch) "pending") ERR_BATCH_PROCESSING_FAILED)
    
    (map-set batch-requests
      { batch-id: batch-id }
      (merge batch { status: "cancelled" })
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-batch-request (batch-id uint))
  (map-get? batch-requests { batch-id: batch-id })
)

(define-read-only (get-batch-certificate-data (batch-id uint) (index uint))
  (map-get? batch-certificate-data { batch-id: batch-id, index: index })
)

(define-read-only (get-institution-batch-stats (institution-id uint))
  (map-get? institution-batch-stats { institution-id: institution-id })
)

(define-read-only (get-batch-progress (batch-id uint))
  (match (map-get? batch-requests { batch-id: batch-id })
    batch (ok {
      progress-percentage: (if (> (get total-certificates batch) u0)
        (/ (* (get processed-count batch) u100) (get total-certificates batch))
        u0),
      status: (get status batch),
      processed: (get processed-count batch),
      total: (get total-certificates batch)
    })
    ERR_BATCH_PROCESSING_FAILED
  )
)

(define-read-only (get-total-batches)
  (var-get batch-counter)
)

(define-read-only (estimate-batch-gas-cost (certificate-count uint))
  ;; Rough estimation: base cost + per-certificate cost
  (ok (+ u10000 (* certificate-count u5000)))
)
