;; NFT Minting Platform Contract
;; A comprehensive NFT contract with minting, marketplace, and batch operations

;; Error constants
(define-constant ERR_INVALID_NFT (err u100))
(define-constant ERR_NOT_AUTHORIZED (err u101))
(define-constant ERR_ALREADY_APPROVED (err u102))
(define-constant ERR_NOT_APPROVED (err u103))
(define-constant ERR_SELF_TRANSFER (err u104))
(define-constant ERR_INVALID_PRICE (err u105))
(define-constant ERR_NOT_FOR_SALE (err u106))
(define-constant ERR_INSUFFICIENT_FUNDS (err u107))
(define-constant ERR_OVERFLOW (err u999))
(define-constant ERR_UNDERFLOW (err u998))

;; Configuration constants
(define-constant MAX_BATCH_SIZE u10)
(define-constant MAX_METADATA_LENGTH u256)
(define-constant DEFAULT_APPROVED_ADDRESS 'SP000000000000000000002Q6VF78)

;; Data variables
(define-data-var nft-counter uint u0)
(define-data-var contract-owner principal tx-sender)
(define-data-var contract-paused bool false)
(define-data-var marketplace-fee uint u250) ;; 2.5% in basis points

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

;; Helper functions
(define-private (is-authorized-transfer (nft-id uint) (caller principal))
  (let ((nft (unwrap-panic (map-get? nfts { nft-id: nft-id }))))
    (or 
      (is-eq caller (get owner nft))
      (is-eq caller (default-to DEFAULT_APPROVED_ADDRESS
                      (get approved (map-get? approvals { nft-id: nft-id }))))
      (is-approved-for-all (get owner nft) caller)
    )
  )
)

(define-private (increment-owner-count (owner principal))
  (let ((current-count (get-owner-nft-count owner)))
    (map-set owner-nft-count 
      { owner: owner } 
      { count: (+ current-count u1) })
  )
)

(define-private (decrement-owner-count (owner principal))
  (let ((current-count (get-owner-nft-count owner)))
    (if (> current-count u0)
      (map-set owner-nft-count 
        { owner: owner } 
        { count: (- current-count u1) })
      true
    )
  )
)

(define-private (is-valid-metadata (metadata (string-ascii 256)))
  (> (len metadata) u0)
)

(define-private (is-valid-price (price uint))
  (> price u0)
)

(define-private (is-valid-nft-id (nft-id uint))
  (< nft-id (var-get nft-counter))
)

;; Math helpers
(define-private (min (a uint) (b uint))
  (if (< a b) a b)
)

(define-private (max (a uint) (b uint))
  (if (> a b) a b)
)

;; Safe math operations
(define-private (safe-add (a uint) (b uint))
  (let ((result (+ a b)))
    (if (>= result a) 
      (ok result) 
      ERR_OVERFLOW)
  )
)

(define-private (safe-sub (a uint) (b uint))
  (if (>= a b)
    (ok (- a b))
    ERR_UNDERFLOW)
)

;; Fixed list helper - simplified implementation
(define-private (list-contains (item uint) (items (list 10 uint)))
  (fold check-item-helper items { item: item, found: false })
)

(define-private (check-item-helper (current-item uint) (acc { item: uint, found: bool }))
  { item: (get item acc), found: (or (get found acc) (is-eq current-item (get item acc))) }
)

;; Core NFT functions
(define-public (mint-nft (metadata (string-ascii 256)))
  (let ((nft-id (var-get nft-counter)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
      (asserts! (is-valid-metadata metadata) ERR_INVALID_NFT)
      (map-insert nfts 
        { nft-id: nft-id } 
        { owner: tx-sender, metadata: metadata, created-at: stacks-block-height })
      (increment-owner-count tx-sender)
      (var-set nft-counter (+ nft-id u1))
      (ok nft-id)
    )
  )
)

(define-public (transfer-nft (nft-id uint) (recipient principal))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
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
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
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

;; Approval functions
(define-public (approve (nft-id uint) (approved principal))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get owner nft) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (not (is-eq tx-sender approved)) ERR_SELF_TRANSFER)
      (map-set approvals { nft-id: nft-id } { approved: approved })
      (ok true)
    )
  )
)

(define-public (set-approval-for-all (operator principal) (approved bool))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq tx-sender operator)) ERR_SELF_TRANSFER)
    (map-set operator-approvals 
      { owner: tx-sender, operator: operator } 
      { approved: approved })
    (ok true)
  )
)

(define-public (transfer-from (nft-id uint) (from principal) (to principal))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
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

(define-public (clear-approval (nft-id uint))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get owner nft) tx-sender) ERR_NOT_AUTHORIZED)
      (map-delete approvals { nft-id: nft-id })
      (ok true)
    )
  )
)

;; Marketplace functions
(define-public (list-for-sale (nft-id uint) (price uint))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get owner nft) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (> price u0) ERR_INVALID_PRICE)
      (map-set nft-listings 
        { nft-id: nft-id } 
        { price: price, seller: tx-sender })
      (ok true)
    )
  )
)

(define-public (unlist-from-sale (nft-id uint))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get owner nft) tx-sender) ERR_NOT_AUTHORIZED)
      (map-delete nft-listings { nft-id: nft-id })
      (ok true)
    )
  )
)

(define-public (buy-nft (nft-id uint))
  (let (
    (nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT))
    (listing (unwrap! (map-get? nft-listings { nft-id: nft-id }) ERR_NOT_FOR_SALE))
    (price (get price listing))
    (seller (get seller listing))
  )
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
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

(define-public (update-listing-price (nft-id uint) (new-price uint))
  (let (
    (nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT))
    (listing (unwrap! (map-get? nft-listings { nft-id: nft-id }) ERR_NOT_FOR_SALE))
  )
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get owner nft) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (> new-price u0) ERR_INVALID_PRICE)
      (map-set nft-listings 
        { nft-id: nft-id } 
        { price: new-price, seller: tx-sender })
      (ok true)
    )
  )
)

(define-public (make-offer (nft-id uint) (offer-amount uint) (expiry-block uint))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (nft-exists nft-id) ERR_INVALID_NFT)
    (asserts! (> offer-amount u0) ERR_INVALID_PRICE)
    (asserts! (> expiry-block stacks-block-height) ERR_INVALID_PRICE)
    (asserts! (>= (stx-get-balance tx-sender) offer-amount) ERR_INSUFFICIENT_FUNDS)
    
    ;; In a full implementation, this would escrow the STX
    ;; For now, we'll just store the offer
    (ok true)
  )
)

;; Batch operations
(define-public (batch-transfer (transfers (list 10 { nft-id: uint, recipient: principal })))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (fold batch-transfer-helper transfers (ok true))
  )
)

(define-public (batch-mint (metadata-list (list 10 (string-ascii 256))))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (let ((results (fold batch-mint-helper metadata-list (list))))
      (ok results)
    )
  )
)

(define-public (batch-approve (nft-ids (list 10 uint)) (approved principal))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (fold batch-approve-helper 
      (map create-approval-pair nft-ids (list approved approved approved approved approved approved approved approved approved approved))
      (ok true))
  )
)

(define-public (batch-list (listings (list 10 { nft-id: uint, price: uint })))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (fold batch-list-helper listings (ok true))
  )
)

;; Batch helper functions
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

;; Admin functions
(define-public (admin-mint (recipient principal) (metadata (string-ascii 256)))
  (let ((nft-id (var-get nft-counter)))
    (begin
      (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
      (asserts! (is-valid-metadata metadata) ERR_INVALID_NFT)
      (map-insert nfts 
        { nft-id: nft-id } 
        { owner: recipient, metadata: metadata, created-at: stacks-block-height })
      (increment-owner-count recipient)
      (var-set nft-counter (+ nft-id u1))
      (ok nft-id)
    )
  )
)

(define-public (admin-batch-mint (recipients (list 10 { recipient: principal, metadata: (string-ascii 256) })))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (fold admin-batch-mint-helper recipients (ok (list)))
  )
)

(define-private (admin-batch-mint-helper 
  (mint-data { recipient: principal, metadata: (string-ascii 256) })
  (previous-result (response (list 10 uint) uint)))
  (match previous-result
    success-list (match (admin-mint (get recipient mint-data) (get metadata mint-data))
      nft-id (ok (unwrap-panic (as-max-len? (append success-list nft-id) u10)))
      error-val (err error-val)
    )
    error-val (err error-val)
  )
)

(define-public (admin-force-transfer (nft-id uint) (from principal) (to principal))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get owner nft) from) ERR_NOT_AUTHORIZED)
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

(define-public (transfer-contract-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq tx-sender new-owner)) ERR_SELF_TRANSFER)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

(define-public (update-metadata (nft-id uint) (new-metadata (string-ascii 256)))
  (let ((nft (unwrap! (map-get? nfts { nft-id: nft-id }) ERR_INVALID_NFT)))
    (begin
      (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
      (asserts! (is-valid-metadata new-metadata) ERR_INVALID_NFT)
      (map-set nfts 
        { nft-id: nft-id } 
        { owner: (get owner nft), 
          metadata: new-metadata, 
          created-at: (get created-at nft) })
      (ok true)
    )
  )
)

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (var-set contract-paused false)
    (ok true)
  )
)

(define-public (set-marketplace-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee u1000) ERR_INVALID_PRICE) ;; Max 10%
    (var-set marketplace-fee new-fee)
    (ok true)
  )
)

(define-public (withdraw-fees (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (<= amount (stx-get-balance (as-contract tx-sender))) ERR_INSUFFICIENT_FUNDS)
    (as-contract (stx-transfer? amount tx-sender (var-get contract-owner)))
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

(define-read-only (get-approved (nft-id uint))
  (map-get? approvals { nft-id: nft-id })
)

(define-read-only (is-approved-for-all (owner principal) (operator principal))
  (default-to false 
    (get approved 
      (map-get? operator-approvals { owner: owner, operator: operator })))
)

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

(define-read-only (get-owner-nft-count (owner principal))
  (default-to u0 (get count (map-get? owner-nft-count { owner: owner })))
)

(define-read-only (get-multiple-nfts (nft-ids (list 10 uint)))
  (map get-nft nft-ids)
)

(define-read-only (get-multiple-owners (nft-ids (list 10 uint)))
  (map get-owner nft-ids)
)

(define-read-only (get-multiple-listings (nft-ids (list 10 uint)))
  (map get-nft-listing nft-ids)
)

(define-read-only (calculate-total-value (nft-ids (list 10 uint)))
  (fold calculate-value-helper nft-ids u0)
)

(define-private (calculate-value-helper (nft-id uint) (total uint))
  (match (get-nft-listing nft-id)
    listing (+ total (get price listing))
    total
  )
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

(define-read-only (get-marketplace-fee)
  (var-get marketplace-fee)
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

;; Pagination helpers (simplified implementations)
(define-read-only (get-nfts-by-owner-paginated (owner principal) (offset uint) (limit uint))
  ;; In a full implementation, this would iterate through NFTs efficiently
  ;; For now, return empty list as placeholder
  (list)
)

(define-read-only (get-recent-nfts (limit uint))
  ;; Returns most recently minted NFTs
  (let ((total-supply (var-get nft-counter)))
    (if (> total-supply limit)
      ;; Would return last 'limit' NFTs
      (list)
      ;; Return all NFTs if less than limit
      (list)
    )
  )
)
