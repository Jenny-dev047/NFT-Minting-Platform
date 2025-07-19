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