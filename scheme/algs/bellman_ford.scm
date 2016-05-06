;;; BELLMAN-FORD

(define-record-type state
  (fields (immutable num-nodes)
          (mutable updated)
          (mutable dist)
          (mutable parent)
          (mutable round)))

(define (state-init uid num-nodes start-node)
  (let ((dist (if (= uid start-node) 0 '())))
    (make-state num-nodes #t dist '() 0)))

(define (transition state edges-in edges-out gui)
  (state-round-set! state (+ (state-round state) 1))

  (cond ((< (state-round state) (+ 2 (state-num-nodes state)))
         ;; BROADCAST
         (if (state-updated state)
             (begin (msg-spam! edges-out (state-dist state))
                    (state-updated-set! state #f)))
         ;; RELAX
         (for-each
           (lambda (edge)
             (if (not (null? (edge-msg edge)))
                 (let ((dist (+ (edge-weight edge) (edge-msg edge)))
                       (parent (edge-number edge)))
                   (if (or (null? (state-dist state))
                           (< dist (state-dist state)))
                       (begin (state-updated-set! state #t)
                              (state-dist-set!    state dist)
                              (state-parent-set!  state parent))))))
           edges-in)))

  ;; GUI
  (gui-edge-text-all gui edges-out)
  (let ((dist (state-dist state))
        (parent (edge-with-num edges-in (state-parent state))))
    (gui-node-text gui (if (null? dist) "inf" dist))
    (if parent
        (gui-edge-highlight gui parent "blue")))
  (if (eqv? 0 (state-dist state)) ; root node
      (gui-node-highlight gui "blue"))
  )

(define bellman-ford (make-algorithm state-init transition))
(set-algorithm! bellman-ford)

