;; CarbonVault - Carbon credit tracking and verification system
(define-map carbon-credits uint {
  issuer: principal,
  project-name: (string-utf8 64),
  environmental-impact: (string-utf8 256),
  issuance-date: uint,
  project-location: (string-utf8 64),
  impact-verified: bool
})

(define-map issuer-projects principal (list 100 uint))
(define-map environmental-auditors principal bool)
(define-data-var credit-id-sequence uint u0)

;; Error codes
(define-constant err-not-issuer (err u400))
(define-constant err-not-auditor (err u401))
(define-constant err-credit-not-found (err u402))
(define-constant err-unauthorized-access (err u403))
(define-constant err-project-limit-exceeded (err u404))
(define-constant err-invalid-auditor-address (err u405))
(define-constant err-invalid-project-name (err u406))
(define-constant err-invalid-impact-description (err u407))
(define-constant err-invalid-issuance-date (err u408))
(define-constant err-invalid-project-location (err u409))
(define-constant err-invalid-credit-id (err u410))

;; Platform administrator
(define-constant platform-administrator tx-sender)

;; Register environmental auditor
(define-public (register-environmental-auditor (auditor principal))
  (begin
    ;; Check if sender is platform administrator
    (asserts! (is-eq tx-sender platform-administrator) err-unauthorized-access)
    
    ;; Validate auditor principal
    (asserts! (not (is-eq auditor 'SP000000000000000000002Q6VF78)) err-invalid-auditor-address)
    
    ;; Add auditor to registry
    (ok (map-set environmental-auditors auditor true))
  )
)

;; Issue carbon credit
(define-public (issue-carbon-credit 
  (project-name (string-utf8 64)) 
  (environmental-impact (string-utf8 256)) 
  (issuance-date uint) 
  (project-location (string-utf8 64)))
  (let
    ((credit-id (var-get credit-id-sequence))
     (issuer tx-sender)
     (current-projects (default-to (list) (map-get? issuer-projects issuer))))
    
    ;; Validate inputs
    (asserts! (> (len project-name) u0) err-invalid-project-name)
    (asserts! (> (len environmental-impact) u0) err-invalid-impact-description)
    (asserts! (> issuance-date u0) err-invalid-issuance-date)
    (asserts! (> (len project-location) u0) err-invalid-project-location)
    
    ;; Check project limit
    (asserts! (< (len current-projects) u100) err-project-limit-exceeded)
    
    ;; Store carbon credit information
    (map-set carbon-credits credit-id {
      issuer: issuer,
      project-name: project-name,
      environmental-impact: environmental-impact,
      issuance-date: issuance-date,
      project-location: project-location,
      impact-verified: false
    })
    
    ;; Update issuer's project list
    (let 
      ((updated-projects (unwrap-panic (as-max-len? (concat (list credit-id) current-projects) u100))))
      (map-set issuer-projects issuer updated-projects)
    )
    
    ;; Increment credit ID sequence
    (var-set credit-id-sequence (+ credit-id u1))
    
    (ok credit-id)))

;; Verify environmental impact
(define-public (verify-environmental-impact (credit-id uint))
  (begin
    ;; Validate credit ID
    (asserts! (< credit-id (var-get credit-id-sequence)) err-invalid-credit-id)
    
    (let
      ((credit (unwrap! (map-get? carbon-credits credit-id) err-credit-not-found)))
      
      ;; Check if sender is environmental auditor
      (asserts! (default-to false (map-get? environmental-auditors tx-sender)) err-not-auditor)
      
      ;; Update credit verification status
      (ok (map-set carbon-credits credit-id (merge credit {impact-verified: true})))
    )
  )
)

;; Get carbon credit details
(define-read-only (get-carbon-credit-details (credit-id uint))
  (map-get? carbon-credits credit-id))

;; Get issuer's projects
(define-read-only (get-issuer-projects (issuer principal))
  (default-to (list) (map-get? issuer-projects issuer)))

;; Check auditor status
(define-read-only (is-environmental-auditor (address principal))
  (default-to false (map-get? environmental-auditors address)))
