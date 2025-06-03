
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_LOCK_NOT_FOUND (err u103))
(define-constant ERR_LOCK_NOT_EXPIRED (err u104))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u105))
(define-constant ERR_VOTING_ENDED (err u106))
(define-constant ERR_ALREADY_VOTED (err u107))
(define-constant ERR_INVALID_DURATION (err u108))

(define-data-var next-proposal-id uint u1)
(define-data-var governance-token principal .governance-token)

(define-map user-locks
  { user: principal }
  {
    amount: uint,
    lock-height: uint,
    unlock-height: uint,
    voting-power: uint
  }
)

(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    start-height: uint,
    end-height: uint,
    yes-votes: uint,
    no-votes: uint,
    total-voting-power: uint,
    executed: bool
  }
)

(define-map user-votes
  { proposal-id: uint, voter: principal }
  {
    vote: bool,
    voting-power: uint,
    timestamp: uint
  }
)

(define-map user-proposal-votes
  { user: principal, proposal-id: uint }
  bool
)

(define-public (lock-tokens (amount uint) (duration uint))
  (let (
    (current-height stacks-block-height)
    (unlock-height (+ current-height duration))
    (voting-power (calculate-voting-power amount duration))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= duration u144) ERR_INVALID_DURATION)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set user-locks
      { user: tx-sender }
      {
        amount: amount,
        lock-height: current-height,
        unlock-height: unlock-height,
        voting-power: voting-power
      }
    )
    (ok voting-power)
  )
)

(define-public (unlock-tokens)
  (let (
    (lock-data (unwrap! (map-get? user-locks { user: tx-sender }) ERR_LOCK_NOT_FOUND))
    (current-height stacks-block-height)
  )
    (asserts! (>= current-height (get unlock-height lock-data)) ERR_LOCK_NOT_EXPIRED)
    (try! (as-contract (stx-transfer? (get amount lock-data) tx-sender tx-sender)))
    (map-delete user-locks { user: tx-sender })
    (ok (get amount lock-data))
  )
)

(define-public (extend-lock (additional-duration uint))
  (let (
    (lock-data (unwrap! (map-get? user-locks { user: tx-sender }) ERR_LOCK_NOT_FOUND))
    (new-unlock-height (+ (get unlock-height lock-data) additional-duration))
    (total-duration (- new-unlock-height (get lock-height lock-data)))
    (new-voting-power (calculate-voting-power (get amount lock-data) total-duration))
  )
    (asserts! (> additional-duration u0) ERR_INVALID_DURATION)
    (map-set user-locks
      { user: tx-sender }
      (merge lock-data {
        unlock-height: new-unlock-height,
        voting-power: new-voting-power
      })
    )
    (ok new-voting-power)
  )
)

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (voting-duration uint))
  (let (
    (proposal-id (var-get next-proposal-id))
    (start-height stacks-block-height)
    (end-height (+ stacks-block-height voting-duration))
  )
    (asserts! (> voting-duration u0) ERR_INVALID_DURATION)
    (map-set proposals
      { proposal-id: proposal-id }
      {
        title: title,
        description: description,
        proposer: tx-sender,
        start-height: start-height,
        end-height: end-height,
        yes-votes: u0,
        no-votes: u0,
        total-voting-power: u0,
        executed: false
      }
    )
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote (proposal-id uint) (support bool))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
    (user-lock (unwrap! (map-get? user-locks { user: tx-sender }) ERR_LOCK_NOT_FOUND))
    (voting-power (get voting-power user-lock))
    (current-height stacks-block-height)
  )
    (asserts! (<= current-height (get end-height proposal)) ERR_VOTING_ENDED)
    (asserts! (is-none (map-get? user-proposal-votes { user: tx-sender, proposal-id: proposal-id })) ERR_ALREADY_VOTED)
    (map-set user-votes
      { proposal-id: proposal-id, voter: tx-sender }
      {
        vote: support,
        voting-power: voting-power,
        timestamp: current-height
      }
    )
    (map-set user-proposal-votes
      { user: tx-sender, proposal-id: proposal-id }
      true
    )
    (if support
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal {
          yes-votes: (+ (get yes-votes proposal) voting-power),
          total-voting-power: (+ (get total-voting-power proposal) voting-power)
        })
      )
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal {
          no-votes: (+ (get no-votes proposal) voting-power),
          total-voting-power: (+ (get total-voting-power proposal) voting-power)
        })
      )
    )
    (ok voting-power)
  )
)

(define-public (increase-lock-amount (additional-amount uint))
  (let (
    (lock-data (unwrap! (map-get? user-locks { user: tx-sender }) ERR_LOCK_NOT_FOUND))
    (new-amount (+ (get amount lock-data) additional-amount))
    (duration (- (get unlock-height lock-data) (get lock-height lock-data)))
    (new-voting-power (calculate-voting-power new-amount duration))
  )
    (asserts! (> additional-amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? additional-amount tx-sender (as-contract tx-sender)))
    (map-set user-locks
      { user: tx-sender }
      (merge lock-data {
        amount: new-amount,
        voting-power: new-voting-power
      })
    )
    (ok new-voting-power)
  )
)

(define-read-only (get-user-lock (user principal))
  (map-get? user-locks { user: user })
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-user-vote (proposal-id uint) (voter principal))
  (map-get? user-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (has-user-voted (user principal) (proposal-id uint))
  (is-some (map-get? user-proposal-votes { user: user, proposal-id: proposal-id }))
)

(define-read-only (get-voting-power (user principal))
  (match (map-get? user-locks { user: user })
    lock-data (some (get voting-power lock-data))
    none
  )
)

(define-read-only (calculate-voting-power (amount uint) (duration uint))
  (let (
    (base-power amount)
    (time-multiplier (/ duration u144))
    (bonus-power (/ (* base-power time-multiplier) u10))
  )
    (+ base-power bonus-power)
  )
)

(define-read-only (get-proposal-status (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal
    (let (
      (current-height stacks-block-height)
      (is-active (<= current-height (get end-height proposal)))
      (yes-votes (get yes-votes proposal))
      (no-votes (get no-votes proposal))
      (total-votes (get total-voting-power proposal))
    )
      (some {
        active: is-active,
        yes-votes: yes-votes,
        no-votes: no-votes,
        total-votes: total-votes,
        winning: (> yes-votes no-votes)
      })
    )
    none
  )
)

(define-read-only (get-time-remaining (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal
    (let (
      (current-height stacks-block-height)
      (end-height (get end-height proposal))
    )
      (if (<= current-height end-height)
        (some (- end-height current-height))
        (some u0)
      )
    )
    none
  )
)

(define-read-only (get-lock-time-remaining (user principal))
  (match (map-get? user-locks { user: user })
    lock-data
    (let (
      (current-height stacks-block-height)
      (unlock-height (get unlock-height lock-data))
    )
      (if (<= current-height unlock-height)
        (some (- unlock-height current-height))
        (some u0)
      )
    )
    none
  )
)

(define-read-only (get-next-proposal-id)
  (var-get next-proposal-id)
)
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

