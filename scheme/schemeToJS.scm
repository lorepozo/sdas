(load "scheme/fixes.scm")

(define (contract steps)
  (let ((last (list-ref steps (- (length steps) 1))))
    (let lp ((i (- (length steps) 2)))
      (if (< i 0)
        (list last)
        (if (equal? last (list-ref steps i))
            (lp (- i 1))
            (list-head steps (+ i 1)))))))

(define (output-parser steps)
  (let lp ((i 0) (s steps))
    (if (null? s)
        'done
        (let ((step (car s)))
          (let ilp ((v 0))
            (if (> v 3)
                (lp (+ i 1) (cdr s))
                (begin
                  (for-each
                    (lambda (atom)
                      (scheme-to-js (list->js-array atom) v i))
                    (vector-ref step v))
                  (ilp (+ v 1)))))))))

(load "scheme/dist.scm")
(define (input-parser)
  (let* ((inp (js-to-scheme))
         (graph inp))
    (make-alg-args (list->vector (map
                                   (lambda (l)
                                     (list->vector (map
                                                     (lambda (i)
                                                       (if (= 0 i) '() i))
                                                     l)))
                                   graph))
                   15
                   4)))

;(load "scheme/algs/bellman_ford.scm")
(load "scheme/algs/leader_elect.scm")
(output-parser (runtime (input-parser)))

