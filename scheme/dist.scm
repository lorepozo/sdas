;;;; Distributed Algorithm Runtime

(define-record-type algorithm
  (fields (immutable state-init) (immutable trans)))

(define user-algorithm '())

(define (set-algorithm! alg)
  (if (algorithm? alg)
      (set! user-algorithm alg)
      (error "invalid algorithm")))

(define-record-type alg-args
  (fields (immutable graph) (immutable num-rounds) (immutable start-node)))

;;; STATES
;;; makes a vector of states, all initialized, indexed by uid.
(define (init-states alg num-nodes start-node)
  (let ((state-init (algorithm-state-init alg)))
    (list->vector (map (lambda (uid) (state-init uid num-nodes start-node))
                       (iota num-nodes)))))

;;; EDGES
;;; a 4-vector of #(weight, msg, nbr-index, type)
(define (edge? edge)
  (if (vector? edge)
      (= 4 (vector-length edge))
      #f))
(define (edge-weight edge)
  (vector-ref edge 0))
(define (edge-msg edge)
  (vector-ref edge 1))
(define (edge-number edge)
  (vector-ref edge 2))
(define (edge-type edge)
  (vector-ref edge 3))

;;; MESSAGES
;;; makes an NxN matrix of #(weight, '())
(define (make-messages graph)
  (let ((size (vector-length graph)))
    (define (weight i j)
      (vector-ref (vector-ref graph i) j))
    (list->vector
      (map (lambda (i)
             (list->vector
               (map (lambda (j)
                      (vector (weight i j) '()))
                    (iota size))))
           (iota size)))))

(define (msg-num-nodes msgs)
  (vector-length msgs))
(define (msg-get-edge msgs i j)
  (vector-ref (vector-ref msgs i) j))
(define (msg-set-edge! msgs i j msg)
  (vector-set! (vector-ref msgs i) j msg))

(define (msg-to-edges-in msgs j)
  (let ((num-nodes (msg-num-nodes msgs))
        (edges-in '()))
    (define (add-edge weight msg)
      (let ((nbr-idx (length edges-in)))
        (set! edges-in
              (append edges-in (list (vector weight msg nbr-idx 'in))))))
    (let lp ((i 0))
      (if (= i num-nodes)
          edges-in
          (let* ((edge   (msg-get-edge msgs i j))
                 (weight (edge-weight  edge))
                 (msg    (edge-msg     edge)))
            (if (not (null? weight))
                (add-edge weight msg))
            (lp (+ i 1)))))))

(define (msg-to-edges-out msgs i)
  (let ((num-nodes (msg-num-nodes msgs))
        (edges-out '()))
    (define (add-edge weight)
      (let ((nbr-idx (length edges-out)))
        (set! edges-out
              (append edges-out (list (vector weight '() nbr-idx 'out))))))
    (let lp ((j 0))
      (if (= j num-nodes)
          edges-out
          (let ((weight (edge-weight (msg-get-edge msgs i j))))
            (if (not (null? weight))
                (add-edge weight))
            (lp (+ j 1)))))))

(define (msg-read-edges-out! msgs i edges-out)
  (let ((num-nodes (msg-num-nodes msgs))
        (reads     edges-out))
    (let lp ((j 0))
      (if (not (= j num-nodes))
          (let ((weight (edge-weight (msg-get-edge msgs i j))))
            (cond ((not (null? weight))
                   (msg-set-edge! msgs i j (car reads))
                   (set! reads (cdr reads))))
            (lp (+ j 1)))))))

;;; GUI
(define (make-gui graph)
  (list (make-vector 4 '())
        graph
        0))
(define (gui->step gui)
  (car gui))
(define (gui-set-viewer! gui uid)
  (set-car! (cddr gui) uid))
(define (edge->global gui edge)
  (let ((graph     (cadr gui))
        (num-nodes (vector-length (cadr gui)))
        (uid       (caddr gui))
        (nbr-idx   (edge-number edge)))
    (if (eqv? 'out (edge-type edge))
        (let lp ((j 0) (idx nbr-idx))
          (if (= j num-nodes)
              (error "neighbor index too large")
              (if (null? (vector-ref (vector-ref graph uid) j))
                  (lp (+ j 1) idx)
                  (if (= idx 0)
                      (list uid j)
                      (lp (+ j 1) (- idx 1))))))
        (let lp ((i 0) (idx nbr-idx))
          (if (= i num-nodes)
              (error "neighbor index too large")
              (if (null? (vector-ref (vector-ref graph i) uid))
                  (lp (+ i 1) idx)
                  (if (= idx 0)
                      (list i uid)
                      (lp (+ i 1) (- idx 1)))))))))


(define (gui-node-highlight gui color)
  (vector-set! (car gui) 0
               (append (vector-ref (car gui) 0)
                       `((,(caddr gui) ,color))
                       )))
(define (gui-node-text gui text)
  (vector-set! (car gui) 1
               (append (vector-ref (car gui) 1)
                       `((,(caddr gui) ,text))
                       )))
(define (gui-edge-highlight gui edge color)
  (vector-set! (car gui) 2
               (append (vector-ref (car gui) 2)
                       (list (append (edge->global gui edge)
                                     (list color))))))
(define (gui-edge-text gui edge text)
  (vector-set! (car gui) 3
               (append (vector-ref (car gui) 3)
                       (list (append (edge->global gui edge)
                                     (list text))))))

;;; runs a single round. returns (step . new-msgs).
;;; side-effects of updated states.
(define (run-round graph alg states msgs)
  (let ((new-msgs (make-messages graph))
        (gui      (make-gui graph)))
    (let lp ((uid 0))
      (if (= uid (msg-num-nodes msgs))
          (cons (gui->step gui) new-msgs)
          (let ((state     (vector-ref states uid))
                (edges-in  (msg-to-edges-in msgs uid))
                (edges-out (msg-to-edges-out new-msgs uid)))
            (gui-set-viewer! gui uid)
            ((algorithm-trans alg) state edges-in edges-out gui)
            (msg-read-edges-out! new-msgs uid edges-out)
            (lp (+ uid 1)))))
    ))

(define (runtime alg-args)
  (if (algorithm? user-algorithm)
      (run user-algorithm alg-args)
      (error "algorithm not set")))

(define (run algorithm args)
  (let ((graph (alg-args-graph args))
        (num-rounds (alg-args-num-rounds args))
        (start-node (alg-args-start-node args)))
    (let ((states (init-states algorithm (vector-length graph) start-node))
          (steps (make-list num-rounds))
          (messages (make-messages graph)))
      (let lp ((s steps))
        (if (null? s)
            steps
            (let ((round (run-round graph algorithm states messages)))
              (set-car! s (car round))
              (set! messages (cdr round))
              (lp (cdr s))))))))

(load "scheme/utils.scm")
