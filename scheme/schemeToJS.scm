(load "scheme/fixes.scm")

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
(load "scheme/algs/leader_elect.scm")
(define (input-parser)
  (let* ((inp (js-to-scheme))
         (graph inp))
    (make-alg-args (list->vector (map list->vector graph))
                   15
                   0)))
(output-parser (runtime (input-parser)))

