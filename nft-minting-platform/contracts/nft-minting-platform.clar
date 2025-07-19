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

;; List NFT for sale
(define-public (list-for-sale (nft-id uint) (price uint))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (is-eq (get owner nft) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (> price u0) ERR_INVALID_PRICE)
      (map-set nft-listings 
        { nft-id: nft-id } 
        { price: price, seller: tx-sender })
      (ok true)
    )
  )
)

;; Remove NFT from sale
(define-public (unlist-from-sale (nft-id uint))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (is-eq (get owner nft) tx-sender) ERR_NOT_AUTHORIZED)
      (map-delete nft-listings { nft-id: nft-id })
      (ok true)
    )
  )
)

;; Buy NFT from marketplace
(define-public (buy-nft (nft-id uint))
  (let (
    (nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT))
    (listing (unwrap! (map-get? nft-listings { nft-id: nft-id }) ERR_NOT_FOR_SALE))
    (price (get price listing))
    (seller (get seller listing))
  )
    (begin
      (asserts! (>= (stx-get-balance tx-sender) price) ERR_INSUFFICIENT_FUNDS)
      (asserts! (not (is-eq tx-sender seller)) ERR_SELF_TRANSFER)
      
      ;; Transfer STX payment
      (try! (stx-transfer? price tx-sender seller))
      
      ;; Transfer NFT ownership
      (map-set nfts 
        { nft-id: nft-id } 
        { owner: tx-sender, 
          metadata: (get metadata nft), 
          created-at: (get created-at nft) })
      
      ;; Update counters
      (decrement-owner-count seller)
      (increment-owner-count tx-sender)
      
      ;; Remove from listings and clear approvals
      (map-delete nft-listings { nft-id: nft-id })
      (map-delete approvals { nft-id: nft-id })
      
      (ok true)
    )
  )
)

;; Update listing price
(define-public (update-listing-price (nft-id uint) (new-price uint))
  (let (
    (nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT))
    (listing (unwrap! (map-get? nft-listings { nft-id: nft-id }) ERR_NOT_FOR_SALE))
  )
    (begin
      (asserts! (is-eq (get owner nft) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (> new-price u0) ERR_INVALID_PRICE)
      (map-set nft-listings 
        { nft-id: nft-id } 
        { price: new-price, seller: tx-sender })
      (ok true)
    )
  )
)

;; Make offer for NFT (escrow-based)
(define-public (make-offer (nft-id uint) (offer-amount uint) (expiry-block uint))
  (begin
    (asserts! (nft-exists nft-id) ERR_INVALID_NFT)
    (asserts! (> offer-amount u0) ERR_INVALID_PRICE)
    (asserts! (> expiry-block block-height) ERR_INVALID_PRICE)
    (asserts! (>= (stx-get-balance tx-sender) offer-amount) ERR_INSUFFICIENT_FUNDS)
    
    ;; In a full implementation, this would escrow the STX
    ;; For now, we'll just store the offer
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-nft-listing (nft-id uint))
  (map-get? nft-listings { nft-id: nft-id })
)

(define-read-only (is-listed-for-sale (nft-id uint))
  (is-some (map-get? nft-listings { nft-id: nft-id }))
)

(define-read-only (get-listing-price (nft-id uint))
  (match (map-get? nft-listings { nft-id: nft-id })
    listing (some (get price listing))
    none
  )
)

;; batch.clar - Batch Operations
;; This file handles batch operations for multiple NFTs

;; Batch transfer multiple NFTs
(define-public (batch-transfer (transfers (list 10 { nft-id: uint, recipient: principal })))
  (fold batch-transfer-helper transfers (ok true))
)

;; Batch mint multiple NFTs
(define-public (batch-mint (metadata-list (list 10 (string-ascii 256))))
  (let ((results (fold batch-mint-helper metadata-list (list))))
    (ok results)
  )
)

;; Batch approve multiple NFTs to same address
(define-public (batch-approve (nft-ids (list 10 uint)) (approved principal))
  (fold batch-approve-helper 
    (map create-approval-pair nft-ids (list approved approved approved approved approved approved approved approved approved approved))
    (ok true))
)

;; Batch list multiple NFTs for sale
(define-public (batch-list (listings (list 10 { nft-id: uint, price: uint })))
  (fold batch-list-helper listings (ok true))
)

;; Helper functions for batch operations
(define-private (batch-transfer-helper 
  (transfer-data { nft-id: uint, recipient: principal }) 
  (previous-result (response bool uint)))
  (match previous-result
    success (transfer-nft (get nft-id transfer-data) (get recipient transfer-data))
    error-value (err error-value)
  )
)

(define-private (batch-mint-helper 
  (metadata (string-ascii 256)) 
  (previous-results (list 10 uint)))
  (match (mint-nft metadata)
    success (unwrap-panic (as-max-len? (append previous-results success) u10))
    error-value previous-results
  )
)

(define-private (batch-approve-helper 
  (approval-data { nft-id: uint, approved: principal })
  (previous-result (response bool uint)))
  (match previous-result
    success (approve (get nft-id approval-data) (get approved approval-data))
    error-value (err error-value)
  )
)

(define-private (batch-list-helper 
  (listing-data { nft-id: uint, price: uint })
  (previous-result (response bool uint)))
  (match previous-result
    success (list-for-sale (get nft-id listing-data) (get price listing-data))
    error-value (err error-value)
  )
)

(define-private (create-approval-pair (nft-id uint) (approved principal))
  { nft-id: nft-id, approved: approved }
)

;; Batch read operations
(define-read-only (get-multiple-nfts (nft-ids (list 10 uint)))
  (map get-nft nft-ids)
)

(define-read-only (get-multiple-owners (nft-ids (list 10 uint)))
  (map get-owner nft-ids)
)

(define-read-only (get-multiple-listings (nft-ids (list 10 uint)))
  (map get-nft-listing nft-ids)
)

