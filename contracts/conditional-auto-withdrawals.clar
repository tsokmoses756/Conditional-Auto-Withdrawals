(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-condition-not-met (err u104))
(define-constant err-already-withdrawn (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-invalid-condition (err u107))
(define-constant err-not-authorized (err u108))
(define-constant err-escrow-not-active (err u109))
(define-constant err-escrow-completed (err u110))
(define-constant err-timeout-not-reached (err u111))
(define-constant err-savings-locked (err u112))
(define-constant err-early-withdrawal-penalty (err u113))
(define-constant err-invalid-duration (err u114))
(define-constant err-invalid-signers (err u115))
(define-constant err-invalid-threshold (err u116))
(define-constant err-not-signer (err u117))
(define-constant err-already-approved (err u118))
(define-constant err-threshold-not-met (err u119))
(define-constant err-tx-already-executed (err u120))

(define-map withdrawal-conditions
  { condition-id: uint }
  {
    creator: principal,
    beneficiary: principal,
    amount: uint,
    condition-type: (string-ascii 20),
    condition-value: uint,
    current-value: uint,
    is-active: bool,
    is-withdrawn: bool,
    created-at: uint
  }
)

(define-map user-balances
  { user: principal }
  { balance: uint }
)

(define-data-var next-condition-id uint u1)
(define-data-var contract-balance uint u0)

(define-public (deposit (amount uint))
  (let ((current-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender })))))
    (if (> amount u0)
      (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set user-balances 
          { user: tx-sender } 
          { balance: (+ current-balance amount) })
        (var-set contract-balance (+ (var-get contract-balance) amount))
        (ok amount))
      err-invalid-amount)))

(define-public (withdraw-balance (amount uint))
  (let ((current-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender })))))
    (if (and (> amount u0) (>= current-balance amount))
      (begin
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (map-set user-balances 
          { user: tx-sender } 
          { balance: (- current-balance amount) })
        (var-set contract-balance (- (var-get contract-balance) amount))
        (ok amount))
      err-insufficient-funds)))

(define-public (create-condition (beneficiary principal) (amount uint) (condition-type (string-ascii 20)) (condition-value uint))
  (let ((condition-id (var-get next-condition-id))
        (creator-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender })))))
    (if (and (> amount u0) (>= creator-balance amount) (> condition-value u0))
      (begin
        (map-set withdrawal-conditions
          { condition-id: condition-id }
          {
            creator: tx-sender,
            beneficiary: beneficiary,
            amount: amount,
            condition-type: condition-type,
            condition-value: condition-value,
            current-value: u0,
            is-active: true,
            is-withdrawn: false,
            created-at: stacks-block-height
          })
        (map-set user-balances 
          { user: tx-sender } 
          { balance: (- creator-balance amount) })
        (var-set next-condition-id (+ condition-id u1))
        (ok condition-id))
      (err err-invalid-condition))))

(define-public (update-condition-value (condition-id uint) (new-value uint))
  (match (map-get? withdrawal-conditions { condition-id: condition-id })
    condition-data
    (if (is-eq tx-sender contract-owner)
      (begin
        (map-set withdrawal-conditions
          { condition-id: condition-id }
          (merge condition-data { current-value: new-value }))
        (ok new-value))
      (err err-owner-only))
    (err err-not-found)))

(define-public (check-and-withdraw (condition-id uint))
  (match (map-get? withdrawal-conditions { condition-id: condition-id })
    condition-data
    (let ((is-condition-met (is-condition-satisfied condition-data)))
      (if (and 
            (get is-active condition-data)
            (not (get is-withdrawn condition-data))
            is-condition-met)
        (begin
          (try! (as-contract (stx-transfer? (get amount condition-data) tx-sender (get beneficiary condition-data))))
          (map-set withdrawal-conditions
            { condition-id: condition-id }
            (merge condition-data { is-withdrawn: true, is-active: false }))
          (var-set contract-balance (- (var-get contract-balance) (get amount condition-data)))
          (ok (get amount condition-data)))
        (if (not is-condition-met)
          err-condition-not-met
          err-already-withdrawn)))
    err-not-found))

(define-public (cancel-condition (condition-id uint))
  (match (map-get? withdrawal-conditions { condition-id: condition-id })
    condition-data
    (if (and 
          (is-eq tx-sender (get creator condition-data))
          (get is-active condition-data)
          (not (get is-withdrawn condition-data)))
      (let ((creator-balance (default-to u0 (get balance (map-get? user-balances { user: (get creator condition-data) })))))
        (map-set withdrawal-conditions
          { condition-id: condition-id }
          (merge condition-data { is-active: false }))
        (map-set user-balances 
          { user: (get creator condition-data) } 
          { balance: (+ creator-balance (get amount condition-data)) })
        (ok (get amount condition-data)))
      (err err-owner-only))
    (err err-not-found)))

(define-public (force-withdraw (condition-id uint))
  (match (map-get? withdrawal-conditions { condition-id: condition-id })
    condition-data
    (if (and 
          (is-eq tx-sender (get beneficiary condition-data))
          (get is-active condition-data)
          (not (get is-withdrawn condition-data))
          (is-condition-satisfied condition-data))
      (begin
        (try! (as-contract (stx-transfer? (get amount condition-data) tx-sender (get beneficiary condition-data))))
        (map-set withdrawal-conditions
          { condition-id: condition-id }
          (merge condition-data { is-withdrawn: true, is-active: false }))
        (var-set contract-balance (- (var-get contract-balance) (get amount condition-data)))
        (ok (get amount condition-data)))
      err-condition-not-met)
    err-not-found))

(define-private (is-condition-satisfied (condition-data (tuple (creator principal) (beneficiary principal) (amount uint) (condition-type (string-ascii 20)) (condition-value uint) (current-value uint) (is-active bool) (is-withdrawn bool) (created-at uint))))
  (let ((condition-type (get condition-type condition-data))
        (condition-value (get condition-value condition-data))
        (current-value (get current-value condition-data)))
    (if (is-eq condition-type "greater-than")
      (> current-value condition-value)
      (if (is-eq condition-type "less-than")
        (< current-value condition-value)
        (if (is-eq condition-type "equal-to")
          (is-eq current-value condition-value)
          (if (is-eq condition-type "stacks-block-height")
            (>= stacks-block-height condition-value)
            false))))))

(define-read-only (get-condition (condition-id uint))
  (map-get? withdrawal-conditions { condition-id: condition-id }))

(define-read-only (get-user-balance (user principal))
  (default-to u0 (get balance (map-get? user-balances { user: user }))))

(define-read-only (get-user-deposits (user principal))
  (default-to u0 (get balance (map-get? user-deposits { user: user }))))

(define-read-only (get-contract-balance)
  (var-get contract-balance))

(define-read-only (get-next-condition-id)
  (var-get next-condition-id))

(define-read-only (is-condition-ready (condition-id uint))
  (match (map-get? withdrawal-conditions { condition-id: condition-id })
    condition-data
    (and 
      (get is-active condition-data)
      (not (get is-withdrawn condition-data))
      (is-condition-satisfied condition-data))
    false))



(define-constant err-payment-not-due (err u203))
(define-constant err-payment-inactive (err u204))
(define-constant err-invalid-interval (err u205))

(define-map recurring-payments
  { payment-id: uint }
  {
    payer: principal,
    recipient: principal,
    amount: uint,
    interval-blocks: uint,
    next-payment-block: uint,
    total-payments: uint,
    payments-made: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map user-deposits
  { user: principal }
  { balance: uint }
)

(define-data-var next-payment-id uint u1)
(define-data-var total-locked uint u0)

(define-public (deposit-funds (amount uint))
  (let ((current-balance (default-to u0 (get balance (map-get? user-deposits { user: tx-sender })))))
    (if (> amount u0)
      (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set user-deposits 
          { user: tx-sender } 
          { balance: (+ current-balance amount) })
        (ok amount))
      err-invalid-amount)))

(define-public (withdraw-funds (amount uint))
  (let ((current-balance (default-to u0 (get balance (map-get? user-deposits { user: tx-sender })))))
    (if (and (> amount u0) (>= current-balance amount))
      (begin
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (map-set user-deposits 
          { user: tx-sender } 
          { balance: (- current-balance amount) })
        (ok amount))
      err-insufficient-funds)))

(define-public (create-recurring-payment (recipient principal) (amount uint) (interval-blocks uint) (total-payments uint))
  (let ((payment-id (var-get next-payment-id))
        (payer-balance (default-to u0 (get balance (map-get? user-deposits { user: tx-sender }))))
        (required-balance (* amount total-payments)))
    (if (and (> amount u0) (> interval-blocks u0) (> total-payments u0) (>= payer-balance required-balance))
      (begin
        (map-set recurring-payments
          { payment-id: payment-id }
          {
            payer: tx-sender,
            recipient: recipient,
            amount: amount,
            interval-blocks: interval-blocks,
            next-payment-block: (+ stacks-block-height interval-blocks),
            total-payments: total-payments,
            payments-made: u0,
            is-active: true,
            created-at: stacks-block-height
          })
        (map-set user-deposits 
          { user: tx-sender } 
          { balance: (- payer-balance required-balance) })
        (var-set total-locked (+ (var-get total-locked) required-balance))
        (var-set next-payment-id (+ payment-id u1))
        (ok payment-id))
      err-insufficient-funds)))

(define-public (execute-payment (payment-id uint))
  (match (map-get? recurring-payments { payment-id: payment-id })
    payment-data
    (if (and 
          (get is-active payment-data)
          (>= stacks-block-height (get next-payment-block payment-data))
          (< (get payments-made payment-data) (get total-payments payment-data)))
      (let ((new-payments-made (+ (get payments-made payment-data) u1))
            (is-final-payment (is-eq new-payments-made (get total-payments payment-data))))
        (try! (as-contract (stx-transfer? (get amount payment-data) tx-sender (get recipient payment-data))))
        (map-set recurring-payments
          { payment-id: payment-id }
          (merge payment-data {
            payments-made: new-payments-made,
            next-payment-block: (+ (get next-payment-block payment-data) (get interval-blocks payment-data)),
            is-active: (not is-final-payment)
          }))
        (var-set total-locked (- (var-get total-locked) (get amount payment-data)))
        (ok (get amount payment-data)))
      err-payment-not-due)
    err-not-found))

(define-public (cancel-recurring-payment (payment-id uint))
  (match (map-get? recurring-payments { payment-id: payment-id })
    payment-data
    (if (and (is-eq tx-sender (get payer payment-data)) (get is-active payment-data))
      (let ((remaining-payments (- (get total-payments payment-data) (get payments-made payment-data)))
            (refund-amount (* remaining-payments (get amount payment-data)))
            (payer-balance (default-to u0 (get balance (map-get? user-deposits { user: (get payer payment-data) })))))
        (map-set recurring-payments
          { payment-id: payment-id }
          (merge payment-data { is-active: false }))
        (map-set user-deposits 
          { user: (get payer payment-data) } 
          { balance: (+ payer-balance refund-amount) })
        (var-set total-locked (- (var-get total-locked) refund-amount))
        (ok refund-amount))
      err-owner-only)
    err-not-found))

(define-public (batch-execute-payments (payment-ids (list 10 uint)))
  (ok (map execute-payment payment-ids)))

(define-read-only (get-payment-details (payment-id uint))
  (map-get? recurring-payments { payment-id: payment-id }))



(define-read-only (is-payment-due (payment-id uint))
  (match (map-get? recurring-payments { payment-id: payment-id })
    payment-data
    (and 
      (get is-active payment-data)
      (>= stacks-block-height (get next-payment-block payment-data))
      (< (get payments-made payment-data) (get total-payments payment-data)))
    false))

(define-read-only (get-next-payment-id)
  (var-get next-payment-id))

(define-read-only (get-total-locked)
  (var-get total-locked))

(define-read-only (calculate-remaining-value (payment-id uint))
  (match (map-get? recurring-payments { payment-id: payment-id })
    payment-data
    (let ((remaining-payments (- (get total-payments payment-data) (get payments-made payment-data))))
      (some (* remaining-payments (get amount payment-data))))
    none))

(define-map escrow-agreements
  { escrow-id: uint }
  {
    buyer: principal,
    seller: principal,
    amount: uint,
    timeout-block: uint,
    buyer-approved: bool,
    seller-approved: bool,
    is-completed: bool,
    is-active: bool,
    created-at: uint
  }
)

(define-data-var next-escrow-id uint u1)

(define-public (create-escrow (seller principal) (amount uint) (timeout-blocks uint))
  (let ((escrow-id (var-get next-escrow-id))
        (buyer-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender })))))
    (if (and (> amount u0) (>= buyer-balance amount) (> timeout-blocks u0))
      (begin
        (map-set escrow-agreements
          { escrow-id: escrow-id }
          {
            buyer: tx-sender,
            seller: seller,
            amount: amount,
            timeout-block: (+ stacks-block-height timeout-blocks),
            buyer-approved: false,
            seller-approved: false,
            is-completed: false,
            is-active: true,
            created-at: stacks-block-height
          })
        (map-set user-balances 
          { user: tx-sender } 
          { balance: (- buyer-balance amount) })
        (var-set next-escrow-id (+ escrow-id u1))
        (ok escrow-id))
      err-insufficient-funds)))

(define-public (approve-escrow (escrow-id uint))
  (match (map-get? escrow-agreements { escrow-id: escrow-id })
    escrow-data
    (if (and (get is-active escrow-data) (not (get is-completed escrow-data)))
      (let ((is-buyer (is-eq tx-sender (get buyer escrow-data)))
            (is-seller (is-eq tx-sender (get seller escrow-data)))
            (new-buyer-approved (if is-buyer true (get buyer-approved escrow-data)))
            (new-seller-approved (if is-seller true (get seller-approved escrow-data))))
        (if (or is-buyer is-seller)
          (begin
            (map-set escrow-agreements
              { escrow-id: escrow-id }
              (merge escrow-data { 
                buyer-approved: new-buyer-approved, 
                seller-approved: new-seller-approved 
              }))
            (ok true))
          err-not-authorized))
      err-escrow-not-active)
    err-not-found))

(define-public (release-escrow (escrow-id uint))
  (match (map-get? escrow-agreements { escrow-id: escrow-id })
    escrow-data
    (if (and 
          (get is-active escrow-data)
          (not (get is-completed escrow-data))
          (get buyer-approved escrow-data)
          (get seller-approved escrow-data))
      (let ((seller-balance (default-to u0 (get balance (map-get? user-balances { user: (get seller escrow-data) })))))
        (map-set escrow-agreements
          { escrow-id: escrow-id }
          (merge escrow-data { is-completed: true, is-active: false }))
        (map-set user-balances 
          { user: (get seller escrow-data) } 
          { balance: (+ seller-balance (get amount escrow-data)) })
        (ok (get amount escrow-data)))
      err-not-authorized)
    err-not-found))

(define-public (refund-escrow (escrow-id uint))
  (match (map-get? escrow-agreements { escrow-id: escrow-id })
    escrow-data
    (if (and 
          (get is-active escrow-data)
          (not (get is-completed escrow-data))
          (>= stacks-block-height (get timeout-block escrow-data)))
      (let ((buyer-balance (default-to u0 (get balance (map-get? user-balances { user: (get buyer escrow-data) })))))
        (map-set escrow-agreements
          { escrow-id: escrow-id }
          (merge escrow-data { is-completed: true, is-active: false }))
        (map-set user-balances 
          { user: (get buyer escrow-data) } 
          { balance: (+ buyer-balance (get amount escrow-data)) })
        (ok (get amount escrow-data)))
      err-timeout-not-reached)
    err-not-found))

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrow-agreements { escrow-id: escrow-id }))

(define-read-only (get-next-escrow-id)
  (var-get next-escrow-id))

(define-read-only (is-escrow-ready-for-release (escrow-id uint))
  (match (map-get? escrow-agreements { escrow-id: escrow-id })
    escrow-data
    (and 
      (get is-active escrow-data)
      (not (get is-completed escrow-data))
      (get buyer-approved escrow-data)
      (get seller-approved escrow-data))
    false))

(define-read-only (is-escrow-refundable (escrow-id uint))
  (match (map-get? escrow-agreements { escrow-id: escrow-id })
    escrow-data
    (and 
      (get is-active escrow-data)
      (not (get is-completed escrow-data))
      (>= stacks-block-height (get timeout-block escrow-data)))
    false))

(define-map savings-accounts
  { savings-id: uint }
  {
    owner: principal,
    principal-amount: uint,
    lock-duration: uint,
    maturity-block: uint,
    reward-rate: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-data-var next-savings-id uint u1)
(define-data-var total-savings-locked uint u0)

(define-public (create-savings-account (amount uint) (lock-duration uint))
  (let ((savings-id (var-get next-savings-id))
        (owner-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender }))))
        (reward-rate (calculate-reward-rate lock-duration))
        (maturity-block (+ stacks-block-height lock-duration)))
    (if (and (> amount u0) (>= owner-balance amount) (>= lock-duration u1000))
      (begin
        (map-set savings-accounts
          { savings-id: savings-id }
          {
            owner: tx-sender,
            principal-amount: amount,
            lock-duration: lock-duration,
            maturity-block: maturity-block,
            reward-rate: reward-rate,
            is-active: true,
            created-at: stacks-block-height
          })
        (map-set user-balances 
          { user: tx-sender } 
          { balance: (- owner-balance amount) })
        (var-set total-savings-locked (+ (var-get total-savings-locked) amount))
        (var-set next-savings-id (+ savings-id u1))
        (ok savings-id))
      (if (< lock-duration u1000)
        err-invalid-duration
        err-insufficient-funds))))

(define-public (withdraw-savings (savings-id uint))
  (match (map-get? savings-accounts { savings-id: savings-id })
    savings-data
    (if (and (is-eq tx-sender (get owner savings-data)) (get is-active savings-data))
      (let ((is-matured (>= stacks-block-height (get maturity-block savings-data)))
            (principal-amount (get principal-amount savings-data))
            (total-payout (if is-matured 
                            (calculate-total-payout savings-data) 
                            (calculate-early-withdrawal-amount savings-data)))
            (owner-balance (default-to u0 (get balance (map-get? user-balances { user: (get owner savings-data) })))))
        (map-set savings-accounts
          { savings-id: savings-id }
          (merge savings-data { is-active: false }))
        (map-set user-balances 
          { user: (get owner savings-data) } 
          { balance: (+ owner-balance total-payout) })
        (var-set total-savings-locked (- (var-get total-savings-locked) principal-amount))
        (ok total-payout))
      err-not-authorized)
    err-not-found))

(define-private (calculate-reward-rate (duration uint))
  (if (>= duration u5000)
    u10
    (if (>= duration u3000)
      u7
      (if (>= duration u1000)
        u5
        u0))))

(define-private (calculate-total-payout (savings-data (tuple (owner principal) (principal-amount uint) (lock-duration uint) (maturity-block uint) (reward-rate uint) (is-active bool) (created-at uint))))
  (let ((principal (get principal-amount savings-data))
        (rate (get reward-rate savings-data)))
    (+ principal (/ (* principal rate) u100))))

(define-private (calculate-early-withdrawal-amount (savings-data (tuple (owner principal) (principal-amount uint) (lock-duration uint) (maturity-block uint) (reward-rate uint) (is-active bool) (created-at uint))))
  (let ((principal (get principal-amount savings-data))
        (penalty-rate u15))
    (- principal (/ (* principal penalty-rate) u100))))

(define-read-only (get-savings-account (savings-id uint))
  (map-get? savings-accounts { savings-id: savings-id }))

(define-read-only (get-next-savings-id)
  (var-get next-savings-id))

(define-read-only (get-total-savings-locked)
  (var-get total-savings-locked))

(define-read-only (is-savings-matured (savings-id uint))
  (match (map-get? savings-accounts { savings-id: savings-id })
    savings-data
    (and 
      (get is-active savings-data)
      (>= stacks-block-height (get maturity-block savings-data)))
    false))

(define-read-only (calculate-projected-return (savings-id uint))
  (match (map-get? savings-accounts { savings-id: savings-id })
    savings-data
    (if (get is-active savings-data)
      (some (calculate-total-payout savings-data))
      none)
    none))

(define-read-only (get-savings-details-for-display (savings-id uint))
  (match (map-get? savings-accounts { savings-id: savings-id })
    savings-data
    (let ((is-matured (>= stacks-block-height (get maturity-block savings-data)))
          (projected-return (calculate-total-payout savings-data)))
      (some {
        owner: (get owner savings-data),
        amount: (get principal-amount savings-data),
        maturity-block: (get maturity-block savings-data),
        reward-rate: (get reward-rate savings-data),
        is-matured: is-matured,
        projected-return: projected-return,
        is-active: (get is-active savings-data)
      }))
    none))

(define-map multisig-wallets
  { wallet-id: uint }
  {
    signers: (list 10 principal),
    required-approvals: uint,
    balance: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map multisig-transactions
  { tx-id: uint }
  {
    wallet-id: uint,
    recipient: principal,
    amount: uint,
    approvals: (list 10 principal),
    is-executed: bool,
    created-by: principal,
    created-at: uint
  }
)

(define-data-var next-wallet-id uint u1)
(define-data-var next-tx-id uint u1)

(define-public (create-multisig-wallet (signers (list 10 principal)) (required-approvals uint))
  (let ((wallet-id (var-get next-wallet-id))
        (num-signers (len signers)))
    (if (and (> num-signers u0) 
             (<= num-signers u10)
             (> required-approvals u0)
             (<= required-approvals num-signers))
      (begin
        (map-set multisig-wallets
          { wallet-id: wallet-id }
          {
            signers: signers,
            required-approvals: required-approvals,
            balance: u0,
            is-active: true,
            created-at: stacks-block-height
          })
        (var-set next-wallet-id (+ wallet-id u1))
        (ok wallet-id))
      (if (or (is-eq num-signers u0) (> num-signers u10))
        err-invalid-signers
        err-invalid-threshold))))

(define-public (deposit-to-multisig (wallet-id uint) (amount uint))
  (match (map-get? multisig-wallets { wallet-id: wallet-id })
    wallet-data
    (if (and (get is-active wallet-data) (> amount u0))
      (let ((sender-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender })))))
        (if (>= sender-balance amount)
          (begin
            (map-set user-balances 
              { user: tx-sender } 
              { balance: (- sender-balance amount) })
            (map-set multisig-wallets
              { wallet-id: wallet-id }
              (merge wallet-data { balance: (+ (get balance wallet-data) amount) }))
            (ok amount))
          err-insufficient-funds))
      err-invalid-amount)
    err-not-found))

(define-public (propose-multisig-transaction (wallet-id uint) (recipient principal) (amount uint))
  (match (map-get? multisig-wallets { wallet-id: wallet-id })
    wallet-data
    (if (and (get is-active wallet-data)
             (is-signer-of-wallet tx-sender (get signers wallet-data))
             (> amount u0)
             (<= amount (get balance wallet-data)))
      (let ((tx-id (var-get next-tx-id)))
        (map-set multisig-transactions
          { tx-id: tx-id }
          {
            wallet-id: wallet-id,
            recipient: recipient,
            amount: amount,
            approvals: (list tx-sender),
            is-executed: false,
            created-by: tx-sender,
            created-at: stacks-block-height
          })
        (var-set next-tx-id (+ tx-id u1))
        (ok tx-id))
      err-not-authorized)
    err-not-found))

(define-public (approve-multisig-transaction (tx-id uint))
  (match (map-get? multisig-transactions { tx-id: tx-id })
    tx-data
    (match (map-get? multisig-wallets { wallet-id: (get wallet-id tx-data) })
      wallet-data
      (if (and (not (get is-executed tx-data))
               (is-signer-of-wallet tx-sender (get signers wallet-data))
               (not (has-approved tx-sender (get approvals tx-data))))
        (let ((new-approvals (unwrap! (as-max-len? (append (get approvals tx-data) tx-sender) u10) err-invalid-signers)))
          (map-set multisig-transactions
            { tx-id: tx-id }
            (merge tx-data { approvals: new-approvals }))
          (ok true))
        (if (has-approved tx-sender (get approvals tx-data))
          err-already-approved
          err-not-signer))
      err-not-found)
    err-not-found))

(define-public (execute-multisig-transaction (tx-id uint))
  (match (map-get? multisig-transactions { tx-id: tx-id })
    tx-data
    (match (map-get? multisig-wallets { wallet-id: (get wallet-id tx-data) })
      wallet-data
      (if (and (not (get is-executed tx-data))
               (>= (len (get approvals tx-data)) (get required-approvals wallet-data))
               (>= (get balance wallet-data) (get amount tx-data)))
        (let ((recipient-balance (default-to u0 (get balance (map-get? user-balances { user: (get recipient tx-data) })))))
          (map-set multisig-transactions
            { tx-id: tx-id }
            (merge tx-data { is-executed: true }))
          (map-set multisig-wallets
            { wallet-id: (get wallet-id tx-data) }
            (merge wallet-data { balance: (- (get balance wallet-data) (get amount tx-data)) }))
          (map-set user-balances
            { user: (get recipient tx-data) }
            { balance: (+ recipient-balance (get amount tx-data)) })
          (ok (get amount tx-data)))
        (if (get is-executed tx-data)
          err-tx-already-executed
          err-threshold-not-met))
      err-not-found)
    err-not-found))

(define-private (is-signer-of-wallet (user principal) (signers (list 10 principal)))
  (is-some (index-of signers user)))

(define-private (has-approved (user principal) (approvals (list 10 principal)))
  (is-some (index-of approvals user)))

(define-read-only (get-multisig-wallet (wallet-id uint))
  (map-get? multisig-wallets { wallet-id: wallet-id }))

(define-read-only (get-multisig-transaction (tx-id uint))
  (map-get? multisig-transactions { tx-id: tx-id }))

(define-read-only (get-next-wallet-id)
  (var-get next-wallet-id))

(define-read-only (get-next-tx-id)
  (var-get next-tx-id))

(define-read-only (is-transaction-ready (tx-id uint))
  (match (map-get? multisig-transactions { tx-id: tx-id })
    tx-data
    (match (map-get? multisig-wallets { wallet-id: (get wallet-id tx-data) })
      wallet-data
      (and (not (get is-executed tx-data))
           (>= (len (get approvals tx-data)) (get required-approvals wallet-data)))
      false)
    false))

(define-read-only (get-transaction-approval-count (tx-id uint))
  (match (map-get? multisig-transactions { tx-id: tx-id })
    tx-data
    (some (len (get approvals tx-data)))
    none))