
;; title: job_listing_and_bidding
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

;; Job Listing and Bidding Contract

;; Constants
;; Job Listing and Bidding Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-bid (err u103))

;; Data structures
(define-map job-listings
  { job-id: uint }
  {
    client: principal,
    title: (string-ascii 100),
    description: (string-utf8 1000),
    budget: uint,
    deadline: uint,
    status: (string-ascii 20)
  }
)

(define-map bids
  { job-id: uint, bidder: principal }
  {
    amount: uint,
    reputation-stake: uint
  }
)

(define-map escrows
  { job-id: uint }
  {
    amount: uint,
    release-time: uint
  }
)

;; Job listing functions
(define-public (create-job-listing (job-id uint) (title (string-ascii 100)) (description (string-utf8 1000)) (budget uint) (deadline uint))
  (let ((job {
               client: tx-sender,
               title: title,
               description: description,
               budget: budget,
               deadline: deadline,
               status: "open" }))
    (if (is-none (map-get? job-listings { job-id: job-id }))
      (begin
        (map-set job-listings { job-id: job-id } job)
        (ok true))
      err-already-exists)))

(define-read-only (get-job-listing (job-id uint))
  (map-get? job-listings { job-id: job-id }))

;; Bidding functions
(define-public (place-bid (job-id uint) (amount uint) (reputation-stake uint))
  (let ((job (unwrap! (get-job-listing job-id) err-not-found))
        (bidder tx-sender))
    (asserts! (<= amount (get budget job)) err-invalid-bid)
    (asserts! (> reputation-stake u0) err-invalid-bid)
    (map-set bids { job-id: job-id, bidder: bidder } { amount: amount, reputation-stake: reputation-stake })
    (ok true)))

(define-read-only (get-bid (job-id uint) (bidder principal))
  (map-get? bids { job-id: job-id, bidder: bidder }))

;; Escrow functions
(define-public (create-escrow (job-id uint) (amount uint))
  (let ((job (unwrap! (get-job-listing job-id) err-not-found)))
    (asserts! (is-eq (get client job) tx-sender) err-owner-only)
    (asserts! (is-eq (get status job) "open") err-invalid-bid)
    (map-set escrows { job-id: job-id } { amount: amount, release-time: (+ block-height u1000) })
    (map-set job-listings { job-id: job-id } 
      (merge job { status: "in-progress" }))
    (ok true)))

(define-public (release-escrow (job-id uint))
  (let ((escrow (unwrap! (map-get? escrows { job-id: job-id }) err-not-found))
        (job (unwrap! (get-job-listing job-id) err-not-found)))
    (asserts! (is-eq (get client job) tx-sender) err-owner-only)
    (asserts! (>= block-height (get release-time escrow)) err-invalid-bid)
    (map-delete escrows { job-id: job-id })
    (map-set job-listings { job-id: job-id } 
      (merge job { status: "completed" }))
    (ok true)))

;; Helper functions
(define-public (cancel-job-listing (job-id uint))
  (let ((job (unwrap! (get-job-listing job-id) err-not-found)))
    (asserts! (is-eq (get client job) tx-sender) err-owner-only)
    (asserts! (is-eq (get status job) "open") err-invalid-bid)
    (map-delete job-listings { job-id: job-id })
    (ok true)))

(define-public (update-job-status (job-id uint) (new-status (string-ascii 20)))
  (let ((job (unwrap! (get-job-listing job-id) err-not-found)))
    (asserts! (is-eq (get client job) tx-sender) err-owner-only)
    (map-set job-listings { job-id: job-id } 
      (merge job { status: new-status }))
    (ok true)))
