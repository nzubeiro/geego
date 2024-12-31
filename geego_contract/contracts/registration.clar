
;; title: registration
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

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-already-registered (err u101))
(define-constant err-user-not-found (err u102))

;; Define data variables
(define-map users principal 
  {
    name: (string-ascii 50),
    skills: (list 10 (string-ascii 20)),
    portfolio: (string-ascii 256),
    reviews: (list 5 (tuple (reviewer principal) (rating uint) (comment (string-ascii 100)))),
    verified-skills: (list 5 (string-ascii 20))
  }
)

(define-map skill-verifiers (string-ascii 20) (list 5 principal))

;; User registration function
(define-public (register-user (name (string-ascii 50)))
  (let ((user-principal tx-sender))
    (if (is-some (map-get? users user-principal))
      err-already-registered
      (ok (map-set users user-principal 
        {
          name: name,
          skills: (list),
          portfolio: "",
          reviews: (list),
          verified-skills: (list)
        }
      ))
    )
  )
)

;; Update user profile function
(define-public (update-profile 
  (name (string-ascii 50))
  (skills (list 10 (string-ascii 20)))
  (portfolio (string-ascii 256)))
  (let ((user-principal tx-sender))
    (match (map-get? users user-principal)
      user-data (ok (map-set users user-principal 
        (merge user-data 
          {
            name: name,
            skills: skills,
            portfolio: portfolio
          }
        )))
      err-user-not-found
    )
  )
)

;; Add a review function
(define-public (add-review 
  (user principal)
  (rating uint)
  (comment (string-ascii 100)))
  (let ((reviewer tx-sender))
    (match (map-get? users user)
      user-data 
        (let ((updated-reviews (unwrap! (as-max-len? 
          (append (get reviews user-data) 
            {reviewer: reviewer, rating: rating, comment: comment})
          u5) 
          (err u103))))  ;; Error if max reviews reached
          (ok (map-set users user 
            (merge user-data {reviews: updated-reviews}))))
      err-user-not-found
    )
  )
)

;; Skill verification function
(define-public (verify-skill (user principal) (skill (string-ascii 20)))
  (let ((verifier tx-sender))
    (match (map-get? users user)
      user-data
        (if (is-some (index-of (default-to (list) (map-get? skill-verifiers skill)) verifier))
          (let ((updated-verified-skills (unwrap! (as-max-len? 
            (append (get verified-skills user-data) skill)
            u5) 
            (err u104))))  ;; Error if max verified skills reached
            (ok (map-set users user 
              (merge user-data {verified-skills: updated-verified-skills}))))
          err-not-authorized)
      err-user-not-found
    )
  )
)

;; Add skill verifier function (only contract owner can add verifiers)
(define-public (add-skill-verifier (skill (string-ascii 20)) (verifier principal))
  (if (is-eq tx-sender contract-owner)
    (let ((current-verifiers (default-to (list) (map-get? skill-verifiers skill))))
      (ok (map-set skill-verifiers skill 
        (unwrap! (as-max-len? (append current-verifiers verifier) u5) 
          (err u105)))))  ;; Error if max verifiers reached
    err-not-authorized
  )
)

;; Get user profile function (read-only)
(define-read-only (get-user-profile (user principal))
  (map-get? users user)
)

;; Get skill verifiers function (read-only)
(define-read-only (get-skill-verifiers (skill (string-ascii 20)))
  (map-get? skill-verifiers skill)
)