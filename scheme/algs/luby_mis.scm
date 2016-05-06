;;; LUBY'S MAXIMAL INDEPENDENT SET

(define-record-type state
  (fields (immutable num-nodes)
          (mutable r)
          (mutable active)
          (mutable status)
          (mutable round)))

(define (state-init uid num-nodes start-node)
  (make-state num-nodes 0 #t '() 0))

(define (transition state edges-in edges-out gui)
  (state-round-set! state (+ (state-round state) 1))

  (if (state-active state)
    (cond
      ;; ROUND 1
      ((= 1 (mod (state-round state) 2))
       (if (> (length (edges->msgs edges-in)) 0)
           (state-status-set! state 'loser))
       (if (not (null? (state-status state)))
           (state-active-set! state #f)
           (let ((r (random-integer (expt (state-num-nodes state) 5))))
             (state-r-set! state r)
             (msg-spam! edges-out r)
             (gui-node-text gui r))))
      ;; ROUND 2
      (else
       (let ((max-r (apply max (edges->msgs edges-in))))
         (if (or (null? max-r)
                 (> (state-r state) max-r))
             (state-status-set! state 'winner)))
       (if (eqv? 'winner (state-status state))
           (begin (msg-spam! edges-out 'winner)
                  (gui-edge-highlight-all gui edges-out "green")
                  )))))

  ;; GUI
  (let ((status (state-status state)))
    (if (not (null? status))
        (gui-node-highlight gui (if (eqv? status 'winner)
                                    "green"
                                    "lightgray"))))
  )

(define luby-mis (make-algorithm state-init transition))
(set-algorithm! luby-mis)

