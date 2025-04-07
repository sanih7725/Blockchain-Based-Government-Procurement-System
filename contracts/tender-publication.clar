;; Tender Publication Contract
;; Manages public procurement opportunities

;; Define data variables
(define-data-var admin principal tx-sender)
(define-map tenders
  { tender-id: (string-ascii 50) }
  {
    title: (string-ascii 100),
    description: (string-utf8 1000),
    category: (string-ascii 50),
    department: (string-ascii 100),
    budget: uint,
    publication-date: uint,
    submission-deadline: uint,
    status: (string-ascii 20),
    document-hash: (buff 64)
  }
)

(define-map tender-amendments
  { tender-id: (string-ascii 50), amendment-id: uint }
  {
    description: (string-utf8 500),
    document-hash: (buff 64),
    publication-date: uint
  }
)

;; Define error codes
(define-constant ERR_NOT_AUTHORIZED u1)
(define-constant ERR_TENDER_EXISTS u2)
(define-constant ERR_TENDER_NOT_FOUND u3)
(define-constant ERR_INVALID_STATUS u4)
(define-constant ERR_INVALID_DEADLINE u5)
(define-constant ERR_AMENDMENT_NOT_FOUND u6)

;; Check if caller is admin
(define-private (is-admin)
  (is-eq tx-sender (var-get admin))
)

;; Create a new tender
(define-public (create-tender
                (tender-id (string-ascii 50))
                (title (string-ascii 100))
                (description (string-utf8 1000))
                (category (string-ascii 50))
                (department (string-ascii 100))
                (budget uint)
                (submission-deadline uint)
                (document-hash (buff 64)))
  (let ((current-time (get-block-info? time (- block-height u1))))
    (asserts! (is-admin) (err ERR_NOT_AUTHORIZED))
    (asserts! (is-none (map-get? tenders {tender-id: tender-id})) (err ERR_TENDER_EXISTS))
    (asserts! (is-some current-time) (err u100))
    (asserts! (> submission-deadline (unwrap-panic current-time)) (err ERR_INVALID_DEADLINE))
    (ok (map-set tenders
          {tender-id: tender-id}
          {
            title: title,
            description: description,
            category: category,
            department: department,
            budget: budget,
            publication-date: (unwrap-panic current-time),
            submission-deadline: submission-deadline,
            status: "open",
            document-hash: document-hash
          }
        ))
  )
)

;; Update tender status (admin only)
(define-public (update-tender-status
                (tender-id (string-ascii 50))
                (status (string-ascii 20)))
  (begin
    (asserts! (is-admin) (err ERR_NOT_AUTHORIZED))
    (asserts! (or (is-eq status "open")
                 (is-eq status "closed")
                 (is-eq status "cancelled")
                 (is-eq status "awarded"))
             (err ERR_INVALID_STATUS))
    (match (map-get? tenders {tender-id: tender-id})
      tender-data (ok (map-set tenders
                {tender-id: tender-id}
                (merge tender-data { status: status })
              ))
      (err ERR_TENDER_NOT_FOUND)
    )
  )
)

;; Add amendment to tender (admin only)
(define-public (add-tender-amendment
                (tender-id (string-ascii 50))
                (amendment-id uint)
                (description (string-utf8 500))
                (document-hash (buff 64)))
  (let ((current-time (get-block-info? time (- block-height u1))))
    (asserts! (is-admin) (err ERR_NOT_AUTHORIZED))
    (asserts! (is-some (map-get? tenders {tender-id: tender-id})) (err ERR_TENDER_NOT_FOUND))
    (asserts! (is-none (map-get? tender-amendments {tender-id: tender-id, amendment-id: amendment-id}))
              (err ERR_TENDER_EXISTS))
    (asserts! (is-some current-time) (err u100))
    (ok (map-set tender-amendments
          {tender-id: tender-id, amendment-id: amendment-id}
          {
            description: description,
            document-hash: document-hash,
            publication-date: (unwrap-panic current-time)
          }
        ))
  )
)

;; Extend tender deadline (admin only)
(define-public (extend-tender-deadline
                (tender-id (string-ascii 50))
                (new-deadline uint))
  (let ((current-time (get-block-info? time (- block-height u1))))
    (asserts! (is-admin) (err ERR_NOT_AUTHORIZED))
    (asserts! (is-some current-time) (err u100))
    (asserts! (> new-deadline (unwrap-panic current-time)) (err ERR_INVALID_DEADLINE))
    (match (map-get? tenders {tender-id: tender-id})
      tender-data (ok (map-set tenders
                {tender-id: tender-id}
                (merge tender-data { submission-deadline: new-deadline })
              ))
      (err ERR_TENDER_NOT_FOUND)
    )
  )
)

;; Read-only functions
(define-read-only (get-tender-info (tender-id (string-ascii 50)))
  (map-get? tenders {tender-id: tender-id})
)

(define-read-only (get-tender-amendment
                   (tender-id (string-ascii 50))
                   (amendment-id uint))
  (map-get? tender-amendments {tender-id: tender-id, amendment-id: amendment-id})
)

;; Change admin (admin only)
(define-public (change-admin (new-admin principal))
  (begin
    (asserts! (is-admin) (err ERR_NOT_AUTHORIZED))
    (ok (var-set admin new-admin))
  )
)
