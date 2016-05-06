;;; LEADER ELECTION

(define-record-type state
  (fields (immutable uid)
          (immutable num-nodes)
          (mutable   maxuid)
          (mutable   status)
          (mutable   round)))

(define (state-init uid num-nodes start-node)
  (make-state uid num-nodes uid '() 0))

(define (transition state edges-in edges-out gui)
  (state-round-set! state (+ (state-round state) 1))

  ;; PHASE 1 --- BROADCAST
  ;; first round:
  (if (= (state-round state) 1)
      (msg-spam! edges-out (state-uid state)))
  ;; rest of the rounds:
  (if (< (state-round state) (state-num-nodes state))
      (let* ((new-max  (apply max (edges->msgs edges-in))))
        (cond ((and (not (null? new-max))
                    (> new-max (state-maxuid state)))
               (state-maxuid-set! state new-max)
               (msg-spam! edges-out new-max)))
        (gui-node-text gui (state-maxuid state))
        (gui-edge-text-all gui edges-out)
        (gui-edge-highlight-all gui edges-out "blue")))

  ;; PHASE 2 --- SET-LEADER
  (if (= (state-round state) (state-num-nodes state))
      (state-status-set! state (if (= (state-maxuid state) (state-uid state))
                                   'leader
                                   'not-leader)))
 
  ;; After completion, show off
  (if (>= (state-round state) (state-num-nodes state))
      (gui-node-highlight gui (if (eqv? 'leader (state-status state))
                                  "red"
                                  "black")))
  )

(define leader-elect (make-algorithm state-init transition))
(set-algorithm! leader-elect)
