# 🗳️ VoteLock - Time-Weighted Voting Mechanism

## 📋 Overview

VoteLock is a Clarity smart contract that implements a time-weighted voting mechanism where users gain more voting power by locking their STX tokens for longer periods. The longer you commit to holding, the more influence you have in governance decisions! 🚀

## ✨ Features

- 🔒 **Token Locking**: Lock STX tokens for specified durations
- ⚡ **Time-Weighted Voting Power**: Longer locks = more voting power
- 📊 **Proposal Creation**: Create governance proposals for community voting
- 🗳️ **Secure Voting**: One vote per user per proposal
- 📈 **Lock Management**: Extend duration or increase locked amount
- 🔓 **Token Unlocking**: Retrieve tokens after lock period expires

## 🎯 Core Mechanics

### Voting Power Calculation
```
Voting Power = Base Amount + (Base Amount × Duration Multiplier ÷ 10)
Duration Multiplier = Lock Duration ÷ 144 blocks
```

### Minimum Lock Duration
- **144 blocks** (~24 hours on Stacks mainnet)

## 🚀 Usage Instructions

### 1. Lock Tokens
```clarity
(contract-call? .VoteLock lock-tokens u1000000 u1440) ;; Lock 1 STX for ~10 days
```

### 2. Create Proposal
```clarity
(contract-call? .VoteLock create-proposal 
  "Increase Block Rewards" 
  "Proposal to increase mining rewards by 10%" 
  u2016) ;; 2 week voting period
```

### 3. Vote on Proposal
```clarity
(contract-call? .VoteLock vote u1 true) ;; Vote YES on proposal #1
```

### 4. Extend Lock Duration
```clarity
(contract-call? .VoteLock extend-lock u720) ;; Add ~5 more days
```

### 5. Increase Lock Amount
```clarity
(contract-call? .VoteLock increase-lock-amount u500000) ;; Add 0.5 STX
```

### 6. Unlock Tokens
```clarity
(contract-call? .VoteLock unlock-tokens) ;; After lock period expires
```

## 📖 Read-Only Functions

- `get-user-lock` - View user's lock details
- `get-proposal` - Get proposal information
- `get-voting-power` - Check user's current voting power
- `get-proposal-status` - View proposal voting results
- `get-time-remaining` - Check remaining voting time
- `get-lock-time-remaining` - Check remaining lock time
- `has-user-voted` - Verify if user voted on proposal

## 🔧 Example Queries

```clarity
;; Check your voting power
(contract-call? .VoteLock get-voting-power tx-sender)

;; View proposal details
(contract-call? .VoteLock get-proposal u1)

;; Check proposal status
(contract-call? .VoteLock get-proposal-status u1)
```

## ⚠️ Important Notes

- Tokens are locked in the contract until the unlock height is reached
- Voting power increases with longer lock durations
- Each user can only vote once per proposal
- Proposals have time-limited voting periods
- Lock duration can be extended but not shortened

## 🛡️ Security Features

- Input validation for all parameters
- Protection against double voting
- Secure token transfers using STX native functions
- Time-based access controls

## 📊 Benefits for Long-term Holders

- **Commitment Rewards**: Longer locks provide exponentially more voting power
- **Governance Influence**: Shape the future of the protocol
- **Anti-Gaming**: Prevents short-term manipulation of voting outcomes
- **Aligned Incentives**: Voters have skin in the game

Ready to participate in decentralized governance? Lock your tokens and make your voice heard! 🎉


