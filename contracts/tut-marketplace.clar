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


(define-constant err-invalid-rating (err u108))
(define-constant err-already-rated (err u109))

(define-map session-ratings uint 
  {
    rated: bool,
    rating: uint
  }
)

(define-public (rate-tutor (session-id uint) (rating uint))
  (let 
    (
      (session (unwrap! (map-get? sessions session-id) err-session-not-found))
      (tutor-data (unwrap! (map-get? tutors (get tutor session)) err-not-tutor))
      (rating-data (default-to {rated: false, rating: u0} (map-get? session-ratings session-id)))
    )
    (asserts! (is-eq (get student session) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status session) "completed") err-not-in-session)
    (asserts! (not (get rated rating-data)) err-already-rated)
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
    
    (let ((new-rating (/ (+ (* (get rating tutor-data) (get total-sessions tutor-data)) rating) (+ (get total-sessions tutor-data) u1))))
      (map-set tutors (get tutor session) (merge tutor-data 
        { 
          rating: new-rating,
          total-sessions: (+ (get total-sessions tutor-data) u1)
        }
      ))
      (map-set session-ratings session-id {rated: true, rating: rating})
      (ok new-rating)
    )
  )
)


(define-constant err-no-content (err u110))

(define-map session-content uint
  {
    materials-url: (string-ascii 256),
    notes: (string-ascii 500)
  }
)

(define-public (add-session-content (session-id uint) (materials-url (string-ascii 256)) (notes (string-ascii 500)))
  (let ((session (unwrap! (map-get? sessions session-id) err-session-not-found)))
    (asserts! (is-eq (get tutor session) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status session) "completed") err-not-in-session)
    (asserts! (not (is-eq materials-url "")) err-no-content)
    (ok (map-set session-content session-id
      {
        materials-url: materials-url,
        notes: notes
      }
    ))
  )
)

(define-read-only (get-session-content (session-id uint))
  (map-get? session-content session-id)
)

(define-constant err-dispute-not-found (err u111))
(define-constant err-dispute-already-exists (err u112))
(define-constant err-dispute-already-resolved (err u113))
(define-constant err-invalid-dispute-reason (err u114))
(define-constant err-dispute-time-expired (err u115))

(define-data-var dispute-window uint u144)

(define-map session-disputes uint
  {
    disputant: principal,
    reason: (string-ascii 10),
    description: (string-ascii 500),
    status: (string-ascii 20),
    resolution: (string-ascii 500),
    created-at: uint,
    resolved-at: uint
  }
)

(define-public (raise-dispute (session-id uint) (reason (string-ascii 10)) (description (string-ascii 500)))
  (let 
    (
      (session (unwrap! (map-get? sessions session-id) err-session-not-found))
      (current-height stacks-block-height)
    )
    (asserts! (is-none (map-get? session-disputes session-id)) err-dispute-already-exists)
    (asserts! (or (is-eq (get student session) tx-sender) (is-eq (get tutor session) tx-sender)) err-unauthorized)
    (asserts! (is-eq (get status session) "completed") err-not-in-session)
    (asserts! (<= (- current-height (get timestamp session)) (var-get dispute-window)) err-dispute-time-expired)
    (asserts! (or (is-eq reason "no-show") (is-eq reason "poor-quality") (is-eq reason "payment") (is-eq reason "other")) err-invalid-dispute-reason)
    
    (ok (map-set session-disputes session-id
      {
        disputant: tx-sender,
        reason: reason,
        description: description,
        status: "pending",
        resolution: "",
        created-at: current-height,
        resolved-at: u0
      }
    ))
  )
)

(define-public (resolve-dispute (session-id uint) (resolution (string-ascii 500)) (refund-student bool))
  (let 
    (
      (dispute (unwrap! (map-get? session-disputes session-id) err-dispute-not-found))
      (session (unwrap! (map-get? sessions session-id) err-session-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status dispute) "pending") err-dispute-already-resolved)
    
    (if refund-student
      (let ((fee (/ (* (get amount session) (var-get platform-fee)) u1000)))
        (try! (as-contract (stx-transfer? (- (get amount session) fee) tx-sender (get student session))))
        (try! (as-contract (stx-transfer? fee tx-sender contract-owner)))
      )
      (let ((fee (/ (* (get amount session) (var-get platform-fee)) u1000)))
        (try! (as-contract (stx-transfer? (- (get amount session) fee) tx-sender (get tutor session))))
        (try! (as-contract (stx-transfer? fee tx-sender contract-owner)))
      )
    )
    
    (map-set session-disputes session-id (merge dispute 
      {
        status: "resolved",
        resolution: resolution,
        resolved-at: stacks-block-height
      }
    ))
    (ok refund-student)
  )
)

(define-public (update-dispute-window (new-window uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= new-window u1) (<= new-window u1008)) err-invalid-amount)
    (ok (var-set dispute-window new-window))
  )
)

(define-read-only (get-dispute (session-id uint))
  (map-get? session-disputes session-id)
)

(define-read-only (get-dispute-window)
  (var-get dispute-window)
)

(define-constant err-invalid-plan (err u116))
(define-constant err-subscription-not-found (err u117))
(define-constant err-subscription-expired (err u118))
(define-constant err-insufficient-sessions (err u119))
(define-constant err-plan-not-found (err u120))

(define-data-var subscription-counter uint u0)

(define-map subscription-plans uint
  {
    tutor: principal,
    sessions-count: uint,
    duration-blocks: uint,
    price: uint,
    discount-rate: uint,
    active: bool
  }
)

(define-map user-subscriptions uint
  {
    student: principal,
    plan-id: uint,
    sessions-remaining: uint,
    expires-at: uint,
    purchased-at: uint
  }
)

(define-public (create-subscription-plan (sessions-count uint) (duration-blocks uint) (discount-rate uint))
  (let 
    (
      (tutor-data (unwrap! (map-get? tutors tx-sender) err-not-tutor))
      (plan-id (+ (var-get subscription-counter) u1))
      (base-price (* (get hourly-rate tutor-data) sessions-count))
      (discounted-price (- base-price (/ (* base-price discount-rate) u100)))
    )
    (asserts! (>= sessions-count u3) err-invalid-plan)
    (asserts! (>= duration-blocks u1440) err-invalid-plan)
    (asserts! (and (>= discount-rate u5) (<= discount-rate u50)) err-invalid-plan)
    (var-set subscription-counter plan-id)
    (ok (map-set subscription-plans plan-id
      {
        tutor: tx-sender,
        sessions-count: sessions-count,
        duration-blocks: duration-blocks,
        price: discounted-price,
        discount-rate: discount-rate,
        active: true
      }
    ))
  )
)

(define-public (deactivate-subscription-plan (plan-id uint))
  (let ((plan (unwrap! (map-get? subscription-plans plan-id) err-plan-not-found)))
    (asserts! (is-eq (get tutor plan) tx-sender) err-unauthorized)
    (ok (map-set subscription-plans plan-id (merge plan { active: false })))
  )
)

(define-public (purchase-subscription (plan-id uint))
  (let 
    (
      (plan (unwrap! (map-get? subscription-plans plan-id) err-plan-not-found))
      (subscription-id (+ (var-get subscription-counter) u1))
      (expires-at (+ stacks-block-height (get duration-blocks plan)))
    )
    (asserts! (get active plan) err-invalid-plan)
    (asserts! (>= (stx-get-balance tx-sender) (get price plan)) err-invalid-amount)
    (try! (stx-transfer? (get price plan) tx-sender (as-contract tx-sender)))
    (var-set subscription-counter subscription-id)
    (ok (map-set user-subscriptions subscription-id
      {
        student: tx-sender,
        plan-id: plan-id,
        sessions-remaining: (get sessions-count plan),
        expires-at: expires-at,
        purchased-at: stacks-block-height
      }
    ))
  )
)

(define-public (book-session-with-subscription (subscription-id uint) (tutor principal))
  (let 
    (
      (subscription (unwrap! (map-get? user-subscriptions subscription-id) err-subscription-not-found))
      (plan (unwrap! (map-get? subscription-plans (get plan-id subscription)) err-plan-not-found))
      (session-id (+ (var-get session-counter) u1))
    )
    (asserts! (is-eq (get student subscription) tx-sender) err-unauthorized)
    (asserts! (is-eq (get tutor plan) tutor) err-unauthorized)
    (asserts! (> (get expires-at subscription) stacks-block-height) err-subscription-expired)
    (asserts! (> (get sessions-remaining subscription) u0) err-insufficient-sessions)
    (var-set session-counter session-id)
    (map-set user-subscriptions subscription-id (merge subscription 
      { sessions-remaining: (- (get sessions-remaining subscription) u1) }
    ))
    (ok (map-set sessions session-id
      {
        tutor: tutor,
        student: tx-sender,
        amount: u0,
        status: "pending",
        timestamp: stacks-block-height
      }
    ))
  )
)

(define-public (release-subscription-payment (subscription-id uint))
  (let 
    (
      (subscription (unwrap! (map-get? user-subscriptions subscription-id) err-subscription-not-found))
      (plan (unwrap! (map-get? subscription-plans (get plan-id subscription)) err-plan-not-found))
      (fee (/ (* (get price plan) (var-get platform-fee)) u1000))
    )
    (asserts! (> (get expires-at subscription) stacks-block-height) err-subscription-expired)
    (try! (as-contract (stx-transfer? (- (get price plan) fee) tx-sender (get tutor plan))))
    (try! (as-contract (stx-transfer? fee tx-sender contract-owner)))
    (ok true)
  )
)

(define-read-only (get-subscription-plan (plan-id uint))
  (map-get? subscription-plans plan-id)
)

(define-read-only (get-user-subscription (subscription-id uint))
  (map-get? user-subscriptions subscription-id)
)

(define-public (complete-session-new (session-id uint))
  (let ((session (unwrap! (map-get? sessions session-id) err-session-not-found)))
    (asserts! (is-eq (get tutor session) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status session) "pending") err-already-completed)
    (ok (map-set sessions session-id (merge session { status: "completed" })))
  )
)

(define-public (release-payment (session-id uint))
  (let 
    (
      (session (unwrap! (map-get? sessions session-id) err-session-not-found))
      (dispute (map-get? session-disputes session-id))
    )
    (asserts! (is-eq (get status session) "completed") err-not-in-session)
    (asserts! (> (- stacks-block-height (get timestamp session)) (var-get dispute-window)) err-dispute-time-expired)
    (asserts! (is-none dispute) err-dispute-already-exists)
    
    (let ((fee (/ (* (get amount session) (var-get platform-fee)) u1000)))
      (try! (as-contract (stx-transfer? (- (get amount session) fee) tx-sender (get tutor session))))
      (try! (as-contract (stx-transfer? fee tx-sender contract-owner)))
      (ok true)
    )
  )
)