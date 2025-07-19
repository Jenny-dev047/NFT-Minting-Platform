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