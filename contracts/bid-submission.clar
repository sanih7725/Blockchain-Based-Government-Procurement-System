;; Bid Submission Contract
;; Securely records and seals proposal details

;; Define data variables
(define-data-var admin principal tx-sender)
(define-map bids
  { tender-id: (string-ascii 50), vendor-id: (string-ascii 50) }
  {
    bid-hash: (buff 64),
    submission-date: uint,
    status: (string-ascii 20),
    bid-amount: uint,
    technical-score: (optional uint),
    financial-score: (optional uint),
    total-score: (optional uint)
  }
)

(define-map bid-documents
  { tender-id: (string-ascii 50), vendor-id: (string-ascii 50), document-type: (string-ascii 50) }
  {
    document-hash: (buff 64),
    upload-date: uint
  }
)

;; Define error codes
(define-constant ERR_NOT_AUTHORIZED u1)
(define-constant ERR_BID_EXISTS u2)
(define-constant ERR_BID_NOT_FOUND u3)
(define-constant ERR_TENDER_CLOSED u4)
(define-constant ERR_DOCUMENT_EXISTS u5)
(define-constant ERR_INVALID_STATUS u6)

;; Check if caller is admin
(define-private (is-admin)
  (is-eq tx-sender (var-get admin))
)

;; Submit a bid for a tender
(define-public (submit-bid
                (tender-id (string-ascii 50))
                (vendor-id (string-ascii 50))
                (bid-hash (buff 64))
                (bid-amount uint))
  (let ((current-time (get-block-info? time (- block-height u1))))
    ;; TODO: Ideally would check vendor registration status and tender deadline
    ;; but for simplicity, we'll skip cross-contract calls
    (asserts! (is-none (map-get? bids {tender-id: tender-id, vendor-id: vendor-id}))
              (err ERR_BID_EXISTS))
    (asserts! (is-some current-time) (err u100))
    (ok (map-set bids
          {tender-id: tender-id, vendor-id: vendor-id}
          {
            bid-hash: bid-hash,
            submission-date: (unwrap-panic current-time),
            status: "submitted",
            bid-amount: bid-amount,
            technical-score: none,
            financial-score: none,
            total-score: none
          }
        ))
  )
)

;; Add document to a bid
(define-public (add-bid-document
                (tender-id (string-ascii 50))
                (vendor-id (string-ascii 50))
                (document-type (string-ascii 50))
                (document-hash (buff 64)))
  (let ((current-time (get-block-info? time (- block-height u1))))
    ;; Check if bid exists and if caller is bid owner
    (match (map-get? bids {tender-id: tender-id, vendor-id: vendor-id})
      bid-data
        (begin
          (asserts! (is-eq tx-sender (get-bid-owner tender-id vendor-id)) (err ERR_NOT_AUTHORIZED))
          (asserts! (is-none (map-get? bid-documents
                                {tender-id: tender-id, vendor-id: vendor-id, document-type: document-type}))
                  (err ERR_DOCUMENT_EXISTS))
          (asserts! (is-some current-time) (err u100))
          (ok (map-set bid-documents
                {tender-id: tender-id, vendor-id: vendor-id, document-type: document-type}
                {
                  document-hash: document-hash,
                  upload-date: (unwrap-panic current-time)
                }
              ))
        )
      (err ERR_BID_NOT_FOUND)
    )
  )
)

;; Placeholder function for getting bid owner (in a real system this would be from vendor contract)
(define-private (get-bid-owner (tender-id (string-ascii 50)) (vendor-id (string-ascii 50)))
  tx-sender
)

;; Evaluate bid (admin only)
(define-public (evaluate-bid
                (tender-id (string-ascii 50))
                (vendor-id (string-ascii 50))
                (technical-score uint)
                (financial-score uint))
  (begin
    (asserts! (is-admin) (err ERR_NOT_AUTHORIZED))
    (match (map-get? bids {tender-id: tender-id, vendor-id: vendor-id})
      bid-data
        (let ((total (+ technical-score financial-score)))
          (ok (map-set bids
                {tender-id: tender-id, vendor-id: vendor-id}
                (merge bid-data {
                  status: "evaluated",
                  technical-score: (some technical-score),
                  financial-score: (some financial-score),
                  total-score: (some total)
                })
              ))
        )
      (err ERR_BID_NOT_FOUND)
    )
  )
)

;; Update bid status (admin only)
(define-public (update-bid-status
                (tender-id (string-ascii 50))
                (vendor-id (string-ascii 50))
                (status (string-ascii 20)))
  (begin
    (asserts! (is-admin) (err ERR_NOT_AUTHORIZED))
    (asserts! (or (is-eq status "submitted")
                 (is-eq status "under-review")
                 (is-eq status "evaluated")
                 (is-eq status "selected")
                 (is-eq status "rejected")
                 (is-eq status "awarded"))
             (err ERR_INVALID_STATUS))
    (match (map-get? bids {tender-id: tender-id, vendor-id: vendor-id})
      bid-data (ok (map-set bids
                {tender-id: tender-id, vendor-id: vendor-id}
                (merge bid-data { status: status })
              ))
      (err ERR_BID_NOT_FOUND)
    )
  )
)

;; Read-only functions
(define-read-only (get-bid-info (tender-id (string-ascii 50)) (vendor-id (string-ascii 50)))
  (map-get? bids {tender-id: tender-id, vendor-id: vendor-id})
)

(define-read-only (get-bid-document-info
                   (tender-id (string-ascii 50))
                   (vendor-id (string-ascii 50))
                   (document-type (string-ascii 50)))
  (map-get? bid-documents {tender-id: tender-id, vendor-id: vendor-id, document-type: document-type})
)

;; Change admin (admin only)
(define-public (change-admin (new-admin principal))
  (begin
    (asserts! (is-admin) (err ERR_NOT_AUTHORIZED))
    (ok (var-set admin new-admin))
  )
)
