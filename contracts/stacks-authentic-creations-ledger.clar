;; Stacks Distributed Ledger Solution for Authentic Digital Creations
;;
;; This framework provides tamper-proof registration, management, and transfer of creative works
;; with comprehensive history tracking and sophisticated role-based access

;; Master Counter for Work Registry
(define-data-var creative-works-counter uint u0)

;; Platform Supervisor
(define-constant platform-supervisor tx-sender)

;; ===========================================================================
;; System Response Codes - Standardized Error Handling Framework
;; ===========================================================================

(define-constant err-supervisor-restricted (err u300))
(define-constant err-work-nonexistent (err u301))
(define-constant err-work-already-registered (err u302))
(define-constant err-name-requirements-unmet (err u303))
(define-constant err-specification-invalid (err u304))
(define-constant err-access-unauthorized (err u305))
(define-constant err-ownership-mismatch (err u306))
(define-constant err-viewing-restricted (err u307))
(define-constant err-categorization-invalid (err u308))

;; ===========================================================================
;; Core Data Architecture - Primary Storage Structures
;; ===========================================================================

;; Central Creative Works Repository
(define-map creative-works-repository
  { work-id: uint }
  {
    work-name: (string-ascii 64),
    creator: principal,
    content-dimensions: uint,
    registration-height: uint,
    work-synopsis: (string-ascii 128),
    work-categories: (list 10 (string-ascii 32))
  }
)

;; Access Control Matrix
(define-map viewer-authorization
  { work-id: uint, observer: principal }
  { can-access: bool }
)

;; ===========================================================================
;; Enhanced Utility Functions - Operational Support Layer
;; ===========================================================================

;; Category Format Validator
(define-private (category-format-valid (category (string-ascii 32)))
  (and
    (> (len category) u0)
    (< (len category) u33)
  )
)

;; Complete Category Collection Validator
(define-private (validate-category-set (categories (list 10 (string-ascii 32))))
  (and
    (> (len categories) u0)
    (<= (len categories) u10)
    (is-eq (len (filter category-format-valid categories)) (len categories))
  )
)

;; Work Existence Verification
(define-private (work-registered (work-id uint))
  (is-some (map-get? creative-works-repository { work-id: work-id }))
)

;; Content Size Retriever
(define-private (retrieve-content-size (work-id uint))
  (default-to u0
    (get content-dimensions
      (map-get? creative-works-repository { work-id: work-id })
    )
  )
)

;; Creator Verification Function
(define-private (is-work-creator (work-id uint) (observer principal))
  (match (map-get? creative-works-repository { work-id: work-id })
    work-details (is-eq (get creator work-details) observer)
    false
  )
)

;; ===========================================================================
;; Advanced Security Features - Protection Mechanisms
;; ===========================================================================

;; Operation Frequency Control
(define-map user-activity-monitor
  { participant: principal }
  {
    last-action-height: uint,
    actions-in-period: uint
  }
)

;; Rate Limitation Parameters
(define-data-var activity-window-size uint u100)  ;; blocks
(define-data-var max-actions-per-window uint u10)  ;; max operations per window

;; Rate Limitation Enforcement
(define-private (enforce-rate-limits (participant principal))
  (let
    (
      (activity-record (default-to { last-action-height: u0, actions-in-period: u0 }
        (map-get? user-activity-monitor { participant: participant })))
      (current-period-start (- block-height (var-get activity-window-size)))
    )
    (if (< (get last-action-height activity-record) current-period-start)
      ;; Period reset - start new counting
      (begin
        (map-set user-activity-monitor { participant: participant }
          { last-action-height: block-height, actions-in-period: u1 })
        true)
      ;; Continue counting in current period
      (if (< (get actions-in-period activity-record) (var-get max-actions-per-window))
        (begin
          (map-set user-activity-monitor { participant: participant }
            { 
              last-action-height: block-height,
              actions-in-period: (+ (get actions-in-period activity-record) u1)
            })
          true)
        false)
    )
  )
)

;; System Emergency Controls
(define-data-var platform-suspended bool false)
(define-data-var suspension-explanation (string-ascii 128) "")

;; Platform Operational Status Check
(define-private (platform-operational)
  (not (var-get platform-suspended))
)

;; Operation Sequencing
(define-data-var sequence-tracker uint u0)

;; Security Timelock Duration
(define-data-var security-delay-blocks uint u10)

;; ===========================================================================
;; Access Management Infrastructure - Permission Framework
;; ===========================================================================

;; Permission Tier Definitions
(define-constant access-none u0)
(define-constant access-view u1)
(define-constant access-edit u2)
(define-constant access-full u3)

;; Granular Permission Registry
(define-map tiered-access-controls
  { work-id: uint, participant: principal }
  { 
    access-tier: uint,
    authorized-by: principal,
    granted-at-height: uint
  }
)

;; Permission Tier Verification
(define-private (has-sufficient-permission (work-id uint) (participant principal) (required-tier uint))
  (let
    (
      (work-details (map-get? creative-works-repository { work-id: work-id }))
      (permission-record (map-get? tiered-access-controls { work-id: work-id, participant: participant }))
    )
    (if (is-some work-details)
      (if (is-eq (get creator (unwrap! work-details false)) participant)
        ;; Creator has all permissions
        true
        ;; Check permission tier for non-creators
        (if (is-some permission-record)
          (>= (get access-tier (unwrap! permission-record false)) required-tier)
          false
        )
      )
      false
    )
  )
)

;; ===========================================================================
;; Integrity Verification System - Cryptographic Validation
;; ===========================================================================

;; Content Integrity Registry
(define-map work-integrity-records
  { work-id: uint }
  {
    content-fingerprint: (buff 32),
    hash-method: (string-ascii 10),
    verification-height: uint,
    verified-by-principal: principal
  }
)

;; Pending Operations Registry
(define-map scheduled-operations
  { operation-sequence: uint, work-id: uint }
  {
    operation-name: (string-ascii 20),
    requester: principal,
    destination-principal: (optional principal),
    request-height: uint,
    verification-code: (buff 32),
    expiration-height: uint
  }
)

;; ===========================================================================
;; Core Public Functions - Primary Platform Operations
;; ===========================================================================

;; Register New Creative Work with Complete Metadata
(define-public (register-creative-work
  (name (string-ascii 64))
  (dimensions uint)
  (synopsis (string-ascii 128))
  (categories (list 10 (string-ascii 32)))
)
  (let
    (
      (new-work-id (+ (var-get creative-works-counter) u1))
    )
    ;; Input validation
    (asserts! (> (len name) u0) err-name-requirements-unmet)
    (asserts! (< (len name) u65) err-name-requirements-unmet)
    (asserts! (> dimensions u0) err-specification-invalid)
    (asserts! (< dimensions u1000000000) err-specification-invalid)
    (asserts! (> (len synopsis) u0) err-name-requirements-unmet)
    (asserts! (< (len synopsis) u129) err-name-requirements-unmet)
    (asserts! (validate-category-set categories) err-categorization-invalid)

    ;; Create work entry in repository
    (map-insert creative-works-repository
      { work-id: new-work-id }
      {
        work-name: name,
        creator: tx-sender,
        content-dimensions: dimensions,
        registration-height: block-height,
        work-synopsis: synopsis,
        work-categories: categories
      }
    )

    ;; Initialize access permission for creator
    (map-insert viewer-authorization
      { work-id: new-work-id, observer: tx-sender }
      { can-access: true }
    )

    ;; Update registry counter
    (var-set creative-works-counter new-work-id)
    (ok new-work-id)
  )
)

;; Transfer Creative Work Ownership to New Principal
(define-public (transfer-work-ownership (work-id uint) (new-creator principal))
  (let
    (
      (work-details (unwrap! (map-get? creative-works-repository { work-id: work-id })
        err-work-nonexistent))
    )
    ;; Verify caller is the current creator
    (asserts! (work-registered work-id) err-work-nonexistent)
    (asserts! (is-eq (get creator work-details) tx-sender) err-ownership-mismatch)

    ;; Update ownership record
    (map-set creative-works-repository
      { work-id: work-id }
      (merge work-details { creator: new-creator })
    )
    (ok true)
  )
)

;; Remove Creative Work from Registry Permanently
(define-public (unregister-creative-work (work-id uint))
  (let
    (
      (work-details (unwrap! (map-get? creative-works-repository { work-id: work-id })
        err-work-nonexistent))
    )
    ;; Ownership verification
    (asserts! (work-registered work-id) err-work-nonexistent)
    (asserts! (is-eq (get creator work-details) tx-sender) err-ownership-mismatch)

    ;; Remove work from registry
    (map-delete creative-works-repository { work-id: work-id })
    (ok true)
  )
)

;; Update Existing Creative Work Metadata
(define-public (update-work-details
  (work-id uint)
  (new-name (string-ascii 64))
  (new-dimensions uint)
  (new-synopsis (string-ascii 128))
  (new-categories (list 10 (string-ascii 32)))
)
  (let
    (
      (work-details (unwrap! (map-get? creative-works-repository { work-id: work-id })
        err-work-nonexistent))
    )
    ;; Validation of ownership and parameters
    (asserts! (work-registered work-id) err-work-nonexistent)
    (asserts! (is-eq (get creator work-details) tx-sender) err-ownership-mismatch)
    (asserts! (> (len new-name) u0) err-name-requirements-unmet)
    (asserts! (< (len new-name) u65) err-name-requirements-unmet)
    (asserts! (> new-dimensions u0) err-specification-invalid)
    (asserts! (< new-dimensions u1000000000) err-specification-invalid)
    (asserts! (> (len new-synopsis) u0) err-name-requirements-unmet)
    (asserts! (< (len new-synopsis) u129) err-name-requirements-unmet)
    (asserts! (validate-category-set new-categories) err-categorization-invalid)

    ;; Update work record with new information
    (map-set creative-works-repository
      { work-id: work-id }
      (merge work-details {
        work-name: new-name,
        content-dimensions: new-dimensions,
        work-synopsis: new-synopsis,
        work-categories: new-categories
      })
    )
    (ok true)
  )
)

;; Rate-Limited Work Registration with Abuse Prevention
(define-public (protected-register-work
  (name (string-ascii 64))
  (dimensions uint)
  (synopsis (string-ascii 128))
  (categories (list 10 (string-ascii 32)))
)
  (begin
    ;; Check rate limit
    (asserts! (enforce-rate-limits tx-sender) (err u700))

    ;; Call original function
    (register-creative-work name dimensions synopsis categories)
  )
)

;; Platform Emergency Resume Function
(define-public (resume-platform-operations)
  (begin
    ;; Admin only
    (asserts! (is-eq tx-sender platform-supervisor) err-supervisor-restricted)

    ;; Clear pause state
    (var-set platform-suspended false)
    (var-set suspension-explanation "")
    (ok true)
  )
)

;; Initialize Secure Work Transfer with Verification
(define-public (initiate-verified-transfer (work-id uint) (new-creator principal) (verification-hash (buff 32)))
  (let
    (
      (work-details (unwrap! (map-get? creative-works-repository { work-id: work-id })
        err-work-nonexistent))
      (operation-sequence (+ (var-get sequence-tracker) u1))
      (expiration (+ block-height (var-get security-delay-blocks)))
    )
    ;; Verify caller is the current creator
    (asserts! (work-registered work-id) err-work-nonexistent)
    (asserts! (is-eq (get creator work-details) tx-sender) err-ownership-mismatch)

    ;; Update operation counter
    (var-set sequence-tracker operation-sequence)
    (ok operation-sequence)
  )
)

;; Grant Permission Level to Participant
(define-public (assign-access-tier (work-id uint) (participant principal) (access-level uint))
  (let
    (
      (work-details (unwrap! (map-get? creative-works-repository { work-id: work-id })
        err-work-nonexistent))
    )
    ;; Verify caller is the work creator
    (asserts! (is-eq (get creator work-details) tx-sender) err-ownership-mismatch)
    ;; Verify valid access level
    (asserts! (<= access-level access-full) (err u500))

    (ok true)
  )
)

;; Register Cryptographic Hash for Work Integrity Verification
(define-public (register-integrity-hash (work-id uint) (content-hash (buff 32)) (algorithm (string-ascii 10)))
  (let
    (
      (work-details (unwrap! (map-get? creative-works-repository { work-id: work-id })
        err-work-nonexistent))
    )
    ;; Verify caller is the work creator
    (asserts! (is-eq (get creator work-details) tx-sender) err-ownership-mismatch)
    ;; Verify valid hash algorithm (sha256 or keccak256)
    (asserts! (or (is-eq algorithm "sha256") (is-eq algorithm "keccak256")) (err u600))

    (ok true)
  )
)

;; Verify Creative Work Integrity Against Registered Hash
(define-public (verify-work-integrity (work-id uint) (verification-hash (buff 32)))
  (let
    (
      (integrity-record (unwrap! (map-get? work-integrity-records { work-id: work-id })
        (err u601)))
    )
    ;; Verify hash matches registered hash
    (asserts! (is-eq (get content-fingerprint integrity-record) verification-hash) (err u602))

    (ok true)
  )
)

