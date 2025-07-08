(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-condition-not-met (err u104))
(define-constant err-already-withdrawn (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-invalid-condition (err u107))

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