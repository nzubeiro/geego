
;; title: escrow_and_payment
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

;; Escrow and Payment Mechanism Contract

;; Define data variables
(define-data-var next-project-id uint u0)
(define-map projects 
  { project-id: uint } 
  { 
    client: principal,
    freelancer: principal,
    total-amount: uint,
    milestones: (list 10 { amount: uint, completed: bool }),
    locked-amount: uint,
    dispute: bool
  }
)

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-already-exists (err u101))
(define-constant err-does-not-exist (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-milestone-not-completed (err u104))
(define-constant err-dispute-active (err u105))

;; Create a new project
(define-public (create-project (freelancer principal) (total-amount uint) (milestones (list 10 { amount: uint, completed: bool })))
  (let ((project-id (var-get next-project-id)))
    (if (>= (len milestones) u1)
      (begin
        (map-set projects
          { project-id: project-id }
          { 
            client: tx-sender,
            freelancer: freelancer,
            total-amount: total-amount,
            milestones: milestones,
            locked-amount: u0,
            dispute: false
          }
        )
        (var-set next-project-id (+ project-id u1))
        (ok project-id)
      )
      err-already-exists
    )
  )
)

;; Lock funds in escrow
(define-public (lock-funds (project-id uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) err-does-not-exist))
    (amount (get total-amount project))
  )
    (if (and 
          (is-eq tx-sender (get client project))
          (is-eq (get locked-amount project) u0)
        )
      (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set projects
          { project-id: project-id }
          (merge project { locked-amount: amount })
        )
        (ok true)
      )
      err-unauthorized
    )
  )
)

;; Complete a milestone
(define-public (complete-milestone (project-id uint) (milestone-index uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) err-does-not-exist))
    (milestones (get milestones project))
  )
    (if (and
          (is-eq tx-sender (get freelancer project))
          (< milestone-index (len milestones))
          (not (get completed (unwrap! (element-at milestones milestone-index) err-does-not-exist)))
        )
      (begin
        (map-set projects
  { project-id: project-id }
  (merge project {
    milestones: (unwrap! (replace-at? milestones milestone-index (merge (unwrap! (element-at milestones milestone-index) err-does-not-exist) { completed: true })) err-does-not-exist)
  })
)
        (ok true)
      )
      err-unauthorized
    )
  )
)

;; Approve and release payment for a milestone
(define-public (approve-milestone (project-id uint) (milestone-index uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) err-does-not-exist))
    (milestones (get milestones project))
    (milestone (unwrap! (element-at milestones milestone-index) err-does-not-exist))
  )
    (if (and
          (is-eq tx-sender (get client project))
          (< milestone-index (len milestones))
          (get completed milestone)
          (not (get dispute project))
        )
      (begin
        (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (get freelancer project))))
        (map-set projects
          { project-id: project-id }
          (merge project {
            locked-amount: (- (get locked-amount project) (get amount milestone))
          })
        )
        (ok true)
      )
      err-milestone-not-completed
    )
  )
)

;; Initiate a dispute
(define-public (initiate-dispute (project-id uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) err-does-not-exist))
  )
    (if (or
          (is-eq tx-sender (get client project))
          (is-eq tx-sender (get freelancer project))
        )
      (begin
        (map-set projects
          { project-id: project-id }
          (merge project { dispute: true })
        )
        (ok true)
      )
      err-unauthorized
    )
  )
)

;; Resolve dispute (only contract owner can do this)
(define-public (resolve-dispute (project-id uint) (client-share uint) (freelancer-share uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) err-does-not-exist))
  )
    (if (and
          (is-eq tx-sender contract-owner)
          (get dispute project)
          (is-eq (+ client-share freelancer-share) (get locked-amount project))
        )
      (begin
        (try! (as-contract (stx-transfer? client-share tx-sender (get client project))))
        (try! (as-contract (stx-transfer? freelancer-share tx-sender (get freelancer project))))
        (map-set projects
          { project-id: project-id }
          (merge project {
            locked-amount: u0,
            dispute: false
          })
        )
        (ok true)
      )
      err-unauthorized
    )
  )
)

;; Read-only function to get project details
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)