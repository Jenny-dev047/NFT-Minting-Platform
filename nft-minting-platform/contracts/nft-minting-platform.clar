;; constants.clar - Error Constants and Configuration
;; This file contains all error constants and configuration values

;; Error constants
(define-constant ERR_INVALID_NFT (err u100))
(define-constant ERR_NOT_AUTHORIZED (err u101))
(define-constant ERR_ALREADY_APPROVED (err u102))
(define-constant ERR_NOT_APPROVED (err u103))
(define-constant ERR_SELF_TRANSFER (err u104))
(define-constant ERR_INVALID_PRICE (err u105))
(define-constant ERR_NOT_FOR_SALE (err u106))
(define-constant ERR_INSUFFICIENT_FUNDS (err u107))

;; Configuration constants
(define-constant MAX_BATCH_SIZE u10)
(define-constant MAX_METADATA_LENGTH u256)
(define-constant DEFAULT_APPROVED_ADDRESS 'SP000000000000000000002Q6VF78)

;; Data variables
(define-data-var nft-counter uint u0)
(define-data-var contract-owner principal tx-sender)

;; Core NFT data
(define-map nfts 
  { nft-id: uint } 
  { owner: principal, metadata: (string-ascii 256), created-at: uint })

;; Approval system maps
(define-map approvals 
  { nft-id: uint } 
  { approved: principal })

(define-map operator-approvals 
  { owner: principal, operator: principal } 
  { approved: bool })

;; Marketplace data
(define-map nft-listings 
  { nft-id: uint } 
  { price: uint, seller: principal })

;; Owner statistics
(define-map owner-nft-count 
  { owner: principal } 
  { count: uint })

;; Core NFT functions
(define-public (mint-nft (metadata (string-ascii 256)))
  (let ((nft-id (var-get nft-counter)))
    (begin
      (map-insert nfts 
        { nft-id: nft-id } 
        { owner: tx-sender, metadata: metadata, created-at: block-height })
      (increment-owner-count tx-sender)
      (var-set nft-counter (+ nft-id u1))
      (ok nft-id)
    )
  )
)

(define-public (transfer-nft (nft-id uint) (recipient principal))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (is-authorized-transfer nft-id tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (not (is-eq tx-sender recipient)) ERR_SELF_TRANSFER)
      
      ;; Update ownership
      (map-set nfts 
        { nft-id: nft-id } 
        { owner: recipient, 
          metadata: (get metadata nft), 
          created-at: (get created-at nft) })
      
      ;; Update counters
      (decrement-owner-count (get owner nft))
      (increment-owner-count recipient)
      
      ;; Clear approvals and listings
      (map-delete approvals { nft-id: nft-id })
      (map-delete nft-listings { nft-id: nft-id })
      
      (ok true)
    )
  )
)

(define-public (burn-nft (nft-id uint))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (is-eq (get owner nft) tx-sender) ERR_NOT_AUTHORIZED)
      
      ;; Remove NFT from storage
      (map-delete nfts { nft-id: nft-id })
      (map-delete approvals { nft-id: nft-id })
      (map-delete nft-listings { nft-id: nft-id })
      
      ;; Update owner count
      (decrement-owner-count tx-sender)
      
      (ok true)
    )
  )
)

;; Read-only functions
(define-read-only (get-nft (nft-id uint))
  (map-get? nfts { nft-id: nft-id })
)

(define-read-only (get-total-supply)
  (var-get nft-counter)
)

(define-read-only (nft-exists (nft-id uint))
  (is-some (map-get? nfts { nft-id: nft-id }))
)

(define-read-only (get-owner (nft-id uint))
  (match (map-get? nfts { nft-id: nft-id })
    nft (some (get owner nft))
    none
  )
)

;; Single NFT approval
(define-public (approve (nft-id uint) (approved principal))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (is-eq (get owner nft) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (not (is-eq tx-sender approved)) ERR_SELF_TRANSFER)
      (map-set approvals { nft-id: nft-id } { approved: approved })
      (ok true)
    )
  )
)

;; Approve operator for all NFTs
(define-public (set-approval-for-all (operator principal) (approved bool))
  (begin
    (asserts! (not (is-eq tx-sender operator)) ERR_SELF_TRANSFER)
    (map-set operator-approvals 
      { owner: tx-sender, operator: operator } 
      { approved: approved })
    (ok true)
  )
)

;; Transfer from approved address
(define-public (transfer-from (nft-id uint) (from principal) (to principal))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (is-eq (get owner nft) from) ERR_NOT_AUTHORIZED)
      (asserts! (is-authorized-transfer nft-id tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (not (is-eq from to)) ERR_SELF_TRANSFER)
      
      ;; Update ownership
      (map-set nfts 
        { nft-id: nft-id } 
        { owner: to, 
          metadata: (get metadata nft), 
          created-at: (get created-at nft) })
      
      ;; Update counters
      (decrement-owner-count from)
      (increment-owner-count to)
      
      ;; Clear approvals and listings
      (map-delete approvals { nft-id: nft-id })
      (map-delete nft-listings { nft-id: nft-id })
      
      (ok true)
    )
  )
)

;; Clear approval for specific NFT
(define-public (clear-approval (nft-id uint))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (is-eq (get owner nft) tx-sender) ERR_NOT_AUTHORIZED)
      (map-delete approvals { nft-id: nft-id })
      (ok true)
    )
  )
)

;; Read-only functions
(define-read-only (get-approved (nft-id uint))
  (map-get? approvals { nft-id: nft-id })
)

(define-read-only (is-approved-for-all (owner principal) (operator principal))
  (default-to false 
    (get approved 
      (map-get? operator-approvals { owner: owner, operator: operator })))
)
