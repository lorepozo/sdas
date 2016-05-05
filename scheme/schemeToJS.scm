(define output (list
		 (list (list (list 1 "red") (list 0 "blue")) 
		       (list (list 1 "test") (list 0 "test"))  
		       (list (list 0 1 "blue")) (list (list 0 1 "0-1"))) 
		 (list (list (list 1 "blue") (list 0 "red"))  
		       (list (list 0 "test2") (list 1 "test2")) 
		       (list (list 0 1 "red"))  
		       (list (list 0 1 "0-foo")))
		 ))


(define (outputparser output)
  (define (innerLoop object innerCounter outerCounter)
    (cond ((pair? object)
	   (scheme-to-js (list->js-array (car object)) innerCounter outerCounter)
	   (innerLoop (cdr object) innerCounter outerCounter))))

  (define (stepLoop step innerCounter outerCounter)
    (cond ((pair? step)
		 (innerLoop (car step) innerCounter outerCounter)
		 (stepLoop (cdr step) (+ innerCounter 1) outerCounter))))

  (define (outerLoop output outerCounter)
    (cond ((pair? output)
		 (stepLoop (car output) 0 outerCounter)
		 (outerLoop (cdr output) (+ outerCounter 1)))))
  
  (outerLoop output 0)
  'done)

(outputparser output)

    
