
;; title: reputation-token
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

;; Reputation Token (RT) - SIP-010 Fungible Token

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-transfer-not-allowed (err u102))

;; Token definitions
(define-fungible-token reputation-token)

;; Data maps
(define-map user-job-history principal 
  {
    completed-jobs: uint,
    total-value: uint,
    average-rating: uint
  }
)

;; SIP-010 Standard Functions

(define-read-only (get-name)
  (ok "Reputation Token")
)

(define-read-only (get-symbol)
  (ok "RT")
)

(define-read-only (get-decimals)
  (ok u6)
)

(define-read-only (get-balance (account principal))
  (ok (ft-get-balance reputation-token account))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply reputation-token))
)

(define-read-only (get-token-uri)
  (ok none)
)

;; Reputation Token Specific Functions

;; Mint reputation tokens based on job completion and metrics
(define-public (mint-reputation-tokens (recipient principal) (job-value uint) (job-difficulty uint) (client-rating uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let
      (
        (current-history (default-to {completed-jobs: u0, total-value: u0, average-rating: u0} (map-get? user-job-history recipient)))
        (new-completed-jobs (+ (get completed-jobs current-history) u1))
        (new-total-value (+ (get total-value current-history) job-value))
        (new-average-rating (/ (+ (* (get average-rating current-history) (get completed-jobs current-history)) client-rating) new-completed-jobs))
        (tokens-to-mint (calculate-tokens-to-mint job-value job-difficulty client-rating))
      )
      (map-set user-job-history recipient 
        {
          completed-jobs: new-completed-jobs,
          total-value: new-total-value,
          average-rating: new-average-rating
        }
      )
      (ft-mint? reputation-token tokens-to-mint recipient)
    )
  )
)

;; Helper function to calculate tokens to mint
(define-private (calculate-tokens-to-mint (job-value uint) (job-difficulty uint) (client-rating uint))
  ;; This is a simplified calculation and can be adjusted based on specific requirements
  (/ (* (+ job-value job-difficulty) client-rating) u100)
)

;; Burn reputation tokens as a penalty
(define-public (burn-reputation-tokens (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ft-burn? reputation-token amount user)
  )
)

;; Freeze reputation tokens
(define-public (freeze-reputation-tokens (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ft-transfer? reputation-token amount contract-owner user)
  )
)

;; Override the transfer function to prevent direct transfers
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (err err-transfer-not-allowed)
)

;; Read-only function to get user's job history
(define-read-only (get-user-job-history (user principal))
  (map-get? user-job-history user)
)