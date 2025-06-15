# 🎯 Conditional Auto Withdrawals

> **Smart contract that enables automatic payments when specific conditions are met** 💰

A Clarity smart contract that allows users to create conditional withdrawal agreements where beneficiaries automatically receive payments when predefined conditions are satisfied. Perfect for learning event-based triggers and automated contract execution! ⚡

## 🚀 Features

- 💳 **Deposit & Withdraw**: Manage your STX balance in the contract
- 📋 **Create Conditions**: Set up conditional payments with various trigger types
- 🔄 **Auto Execution**: Payments trigger automatically when conditions are met
- ⏰ **Expiration System**: Conditions have built-in expiry dates
- 🎛️ **Multiple Condition Types**: Support for various comparison operators
- 📊 **Event Tracking**: Complete audit trail of all contract interactions

## 🛠️ Condition Types

| Type | Description | Example Use Case |
|------|-------------|------------------|
| `greater-than` | Trigger when value > threshold | Price alerts |
| `less-than` | Trigger when value < threshold | Stop-loss orders |
| `equal-to` | Trigger when value = target | Exact match payments |
| `greater-equal` | Trigger when value >= threshold | Minimum thresholds |
| `less-equal` | Trigger when value <= threshold | Maximum limits |
| `block-height` | Trigger at specific block | Time-based releases |

## 📖 Usage

### 1️⃣ Deposit Funds
```clarity
(contract-call? .conditional-auto-withdrawals deposit u1000000)
```

### 2️⃣ Create a Condition
````clarity
(contract-call? .conditional-auto-withdrawals 
  create-condition 
  'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7  ;; beneficiary
  u500000                                        ;; amount (0

