(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-tutor (err u101))
(define-constant err-already-tutor (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-session-not-found (err u104))
(define-constant err-not-in-session (err u105))
(define-constant err-unauthorized (err u106))
(define-constant err-already-completed (err u107))

(define-data-var platform-fee uint u50)
(define-data-var min-session-amount uint u1000000)

(define-map tutors principal 
  {
    hourly-rate: uint,
    rating: uint,
    total-sessions: uint,
    available: bool
  }
)

(define-map sessions uint 
  {
    tutor: principal,
    student: principal,
    amount: uint,
    status: (string-ascii 20),
    timestamp: uint
  }
)

(define-data-var session-counter uint u0)

(define-read-only (get-tutor (tutor principal))
  (map-get? tutors tutor)
)

(define-read-only (get-session (session-id uint))
  (map-get? sessions session-id)
)

(define-public (register-as-tutor (hourly-rate uint))
  (begin
    (asserts! (is-none (map-get? tutors tx-sender)) err-already-tutor)
    (asserts! (>= hourly-rate (var-get min-session-amount)) err-invalid-amount)
    (ok (map-set tutors tx-sender {
      hourly-rate: hourly-rate,
      rating: u0,
      total-sessions: u0,
      available: true
    }))
  )
)

(define-public (update-hourly-rate (new-rate uint))
  (let ((tutor-data (unwrap! (map-get? tutors tx-sender) err-not-tutor)))
    (asserts! (>= new-rate (var-get min-session-amount)) err-invalid-amount)
    (ok (map-set tutors tx-sender (merge tutor-data { hourly-rate: new-rate })))
  )
)

(define-public (toggle-availability)
  (let ((tutor-data (unwrap! (map-get? tutors tx-sender) err-not-tutor)))
    (ok (map-set tutors tx-sender (merge tutor-data { available: (not (get available tutor-data)) })))
  )
)

(define-public (book-session (tutor principal))
  (let 
    (
      (tutor-data (unwrap! (map-get? tutors tutor) err-not-tutor))
      (session-id (+ (var-get session-counter) u1))
    )
    (asserts! (get available tutor-data) err-not-tutor)
    (asserts! (>= (stx-get-balance tx-sender) (get hourly-rate tutor-data)) err-invalid-amount)
    (try! (stx-transfer? (get hourly-rate tutor-data) tx-sender (as-contract tx-sender)))
    (var-set session-counter session-id)
    (ok (map-set sessions session-id {
      tutor: tutor,
      student: tx-sender,
      amount: (get hourly-rate tutor-data),
      status: "pending",
      timestamp: stacks-block-height
    }))
  )
)

(define-public (complete-session (session-id uint))
  (let ((session (unwrap! (map-get? sessions session-id) err-session-not-found)))
    (asserts! (is-eq (get tutor session) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status session) "pending") err-already-completed)
    (let ((fee (/ (* (get amount session) (var-get platform-fee)) u1000)))
      (try! (as-contract (stx-transfer? (- (get amount session) fee) tx-sender (get tutor session))))
      (try! (as-contract (stx-transfer? fee tx-sender contract-owner)))
      (ok (map-set sessions session-id (merge session { status: "completed" })))
    )
  )
)

(define-public (cancel-session (session-id uint))
  (let ((session (unwrap! (map-get? sessions session-id) err-session-not-found)))
    (asserts! (or (is-eq (get student session) tx-sender) (is-eq (get tutor session) tx-sender)) err-unauthorized)
    (asserts! (is-eq (get status session) "pending") err-already-completed)
    (try! (as-contract (stx-transfer? (get amount session) tx-sender (get student session))))
    (ok (map-set sessions session-id (merge session { status: "cancelled" })))
  )
)

(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-amount)
    (ok (var-set platform-fee new-fee))
  )
)