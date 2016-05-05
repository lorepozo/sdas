(define make-list
  (lambda args
    (let ((n (car args))
          (e (if (> (length args) 1) (cadr args) '()))
          (l '()))
      (let lp ((i 0))
        (if (= i n)
            l
            (begin (set! l (append l (list e)))
                   (lp (+ i 1))))))))
(define make-vector
  (lambda args (list->vector (apply make-list args))))

(define (extrema op)
  (lambda args
    (if (= 0 (length args))
        '()
        (fold-left (lambda (a b) (if (op a b) a b))
                   (car args)
                   args))))
(define max (extrema >))
(define min (extrema <))

(define error console-error)
(define assert
  (lambda args (if (not (car args)) (apply error (cdr args)))))
