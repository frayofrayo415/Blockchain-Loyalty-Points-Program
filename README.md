# Blockchain Loyalty Points Program
A decentralized loyalty points system built on Stacks blockchain that enables hotels, airlines, and other partners to issue and manage reward points.

## ✨ Features

- 🏢 Partner registration and management
- 💰 Points issuance with partner-specific multipliers
- 🔄 Point transfers between users
- 💳 Points redemption system
- 📊 Balance tracking per user and partner

## 🚀 Smart Contract Functions

### Administrative Functions
- `set-contract-owner`: Update contract administrator
- `register-partner`: Add new partner with custom multiplier
- `deactivate-partner`: Disable partner participation

### User Functions
- `issue-points`: Partners can issue points to users
- `redeem-points`: Users can spend points with partners
- `transfer-points`: Transfer points to other users

### Read-Only Functions
- `get-partner-info`: View partner details
- `get-user-points`: Check point balance with specific partner
- `get-total-points`: View total points across all partners

## 🛠️ Usage

1. Deploy contract using Clarinet
2. Register partners through contract owner
3. Partners can issue points to users
4. Users can manage their points through provided functions

## ⚙️ Requirements

- Clarinet
- Stacks blockchain wallet
```
