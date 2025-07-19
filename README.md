# NFT Minting Platform

A Clarity smart contract for minting and transferring NFTs on the Stacks blockchain.

## Overview
This project enables users to:
- Mint NFTs with custom metadata (up to 256 characters).
- Transfer NFTs to other users.
- Retrieve NFT details.

## Contract Details
- **File**: `nft-minting.clar`
- **Functions**:
  - `(mint-nft metadata)`: Mints a new NFT with given metadata.
  - `(transfer-nft nft-id recipient)`: Transfers an NFT to another principal.
  - `(get-nft nft-id)`: Retrieves NFT details.

## Getting Started
1. Clone the repository.
2. Run `clarinet check` to verify the contract.
3. Deploy to a Stacks testnet.
4. Build a UI to interact with the NFT minting system.

## Testing
Run tests with:
```bash
clarinet test