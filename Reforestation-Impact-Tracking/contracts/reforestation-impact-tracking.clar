;; Reforestation Impact Tracking Smart Contract
;; Monitor and reward tree planting with verifiable satellite data

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_DATA (err u101))
(define-constant ERR_PROJECT_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_VERIFIED (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_INVALID_COORDINATES (err u105))
(define-constant ERR_VERIFICATION_FAILED (err u106))

;; Data Variables
(define-data-var next-project-id uint u1)
(define-data-var total-trees-planted uint u0)
(define-data-var reward-per-tree uint u100000) ;; 0.1 STX per tree in microSTX
(define-data-var verification-fee uint u50000) ;; 0.05 STX fee for verification

;; Data Maps
(define-map projects
  { project-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    location: (string-ascii 100),
    coordinates: { lat: int, lng: int },
    trees-planted: uint,
    area-size: uint, ;; in square meters
    planting-date: uint,
    verification-status: (string-ascii 20),
    satellite-data-hash: (optional (buff 32)),
    verified-trees: uint,
    rewards-claimed: uint,
    created-at: uint
  }
)

(define-map user-stats
  { user: principal }
  {
    total-projects: uint,
    total-trees: uint,
    total-rewards: uint,
    reputation-score: uint
  }
)

(define-map authorized-verifiers
  { verifier: principal }
  { authorized: bool }
)

(define-map satellite-data
  { data-hash: (buff 32) }
  {
    project-id: uint,
    timestamp: uint,
    tree-count: uint,
    coverage-percentage: uint,
    verified-by: principal
  }
)

;; Public Functions

;; Create a new reforestation project
(define-public (create-project 
  (name (string-ascii 50))
  (location (string-ascii 100))
  (lat int)
  (lng int)
  (trees-planted uint)
  (area-size uint))
  (let 
    (
      (project-id (var-get next-project-id))
      (current-time block-height)
    )
    ;; Validate coordinates (basic range check)
    (asserts! (and (>= lat -900000) (<= lat 900000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= lng -1800000) (<= lng 1800000)) ERR_INVALID_COORDINATES)
    (asserts! (> trees-planted u0) ERR_INVALID_DATA)
    (asserts! (> area-size u0) ERR_INVALID_DATA)
    
    ;; Create project entry
    (map-set projects
      { project-id: project-id }
      {
        owner: tx-sender,
        name: name,
        location: location,
        coordinates: { lat: lat, lng: lng },
        trees-planted: trees-planted,
        area-size: area-size,
        planting-date: current-time,
        verification-status: "pending",
        satellite-data-hash: none,
        verified-trees: u0,
        rewards-claimed: u0,
        created-at: current-time
      }
    )
    
    ;; Update user stats
    (update-user-stats tx-sender trees-planted u0)
    
    ;; Increment next project ID
    (var-set next-project-id (+ project-id u1))
    
    ;; Update total trees planted
    (var-set total-trees-planted (+ (var-get total-trees-planted) trees-planted))
    
    (ok project-id)
  )
)

;; Submit satellite verification data
(define-public (submit-satellite-data
  (project-id uint)
  (data-hash (buff 32))
  (verified-tree-count uint)
  (coverage-percentage uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
      (verifier tx-sender)
    )
    ;; Check if caller is authorized verifier
    (asserts! (default-to false (get authorized (map-get? authorized-verifiers { verifier: verifier }))) ERR_NOT_AUTHORIZED)
    
    ;; Validate data
    (asserts! (<= verified-tree-count (get trees-planted project)) ERR_INVALID_DATA)
    (asserts! (<= coverage-percentage u100) ERR_INVALID_DATA)
    
    ;; Check if already verified
    (asserts! (is-eq (get verification-status project) "pending") ERR_ALREADY_VERIFIED)
    
    ;; Store satellite data
    (map-set satellite-data
      { data-hash: data-hash }
      {
        project-id: project-id,
        timestamp: block-height,
        tree-count: verified-tree-count,
        coverage-percentage: coverage-percentage,
        verified-by: verifier
      }
    )
    
    ;; Update project with verification
    (map-set projects
      { project-id: project-id }
      (merge project {
        verification-status: "verified",
        satellite-data-hash: (some data-hash),
        verified-trees: verified-tree-count
      })
    )
    
    (ok true)
  )
)

;; Claim rewards for verified trees
(define-public (claim-rewards (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
      (reward-amount (* (get verified-trees project) (var-get reward-per-tree)))
      (unclaimed-trees (- (get verified-trees project) (get rewards-claimed project)))
      (unclaimed-reward (* unclaimed-trees (var-get reward-per-tree)))
    )
    ;; Check ownership
    (asserts! (is-eq tx-sender (get owner project)) ERR_NOT_AUTHORIZED)
    
    ;; Check if verified
    (asserts! (is-eq (get verification-status project) "verified") ERR_VERIFICATION_FAILED)
    
    ;; Check if there are unclaimed rewards
    (asserts! (> unclaimed-trees u0) ERR_INVALID_DATA)
    
    ;; Transfer rewards (STX)
    (try! (stx-transfer? unclaimed-reward CONTRACT_OWNER tx-sender))
    
    ;; Update project rewards claimed
    (map-set projects
      { project-id: project-id }
      (merge project {
        rewards-claimed: (get verified-trees project)
      })
    )
    
    ;; Update user stats
    (update-user-stats tx-sender u0 unclaimed-reward)
    
    (ok unclaimed-reward)
  )
)

;; Add authorized verifier (only contract owner)
(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set authorized-verifiers
      { verifier: verifier }
      { authorized: true }
    )
    (ok true)
  )
)

;; Remove authorized verifier (only contract owner)
(define-public (remove-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set authorized-verifiers
      { verifier: verifier }
      { authorized: false }
    )
    (ok true)
  )
)

;; Update reward per tree (only contract owner)
(define-public (update-reward-per-tree (new-reward uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set reward-per-tree new-reward)
    (ok true)
  )
)

;; Deposit funds to contract for rewards
(define-public (deposit-funds (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (ok true)
  )
)

;; Private Functions

;; Update user statistics
(define-private (update-user-stats (user principal) (trees uint) (rewards uint))
  (let
    (
      (current-stats (default-to 
        { total-projects: u0, total-trees: u0, total-rewards: u0, reputation-score: u0 }
        (map-get? user-stats { user: user })))
    )
    (map-set user-stats
      { user: user }
      {
        total-projects: (+ (get total-projects current-stats) u1),
        total-trees: (+ (get total-trees current-stats) trees),
        total-rewards: (+ (get total-rewards current-stats) rewards),
        reputation-score: (calculate-reputation (get total-trees current-stats) trees)
      }
    )
  )
)

;; Calculate reputation score based on trees planted
(define-private (calculate-reputation (current-trees uint) (new-trees uint))
  (let ((total-trees (+ current-trees new-trees)))
    (if (>= total-trees u1000)
      u100
      (if (>= total-trees u500)
        u75
        (if (>= total-trees u100)
          u50
          (if (>= total-trees u50)
            u25
            u10))))))

;; Read-only Functions

;; Get project details
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

;; Get user statistics
(define-read-only (get-user-stats (user principal))
  (map-get? user-stats { user: user })
)

;; Get satellite data
(define-read-only (get-satellite-data (data-hash (buff 32)))
  (map-get? satellite-data { data-hash: data-hash })
)

;; Check if verifier is authorized
(define-read-only (is-authorized-verifier (verifier principal))
  (default-to false (get authorized (map-get? authorized-verifiers { verifier: verifier })))
)

;; Get contract stats
(define-read-only (get-contract-stats)
  {
    total-projects: (- (var-get next-project-id) u1),
    total-trees-planted: (var-get total-trees-planted),
    reward-per-tree: (var-get reward-per-tree),
    verification-fee: (var-get verification-fee)
  }
)

;; Get contract balance
(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

;; Calculate potential rewards for a project
(define-read-only (calculate-potential-rewards (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project
    (let
      (
        (verified-trees (get verified-trees project))
        (claimed-rewards (get rewards-claimed project))
        (unclaimed-trees (- verified-trees claimed-rewards))
      )
      (ok {
        total-verified: verified-trees,
        already-claimed: claimed-rewards,
        unclaimed-trees: unclaimed-trees,
        potential-reward: (* unclaimed-trees (var-get reward-per-tree))
      })
    )
    ERR_PROJECT_NOT_FOUND
  )
)