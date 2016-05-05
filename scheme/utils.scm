;;;; Utilities for Writing Distributed Algorithms

;;; The following three operations are defined in dist.scm:
; (edge-weight edge)
; (edge-msg edge)
; (edge-number edge)

(define (edges->msgs edges)
  (filter (lambda (msg) (not (null? msg)))
          (map edge-msg edges)))

(define (edge-with-num edges num)
  (if (number? num)
      (find (lambda (edge)
              (= (edge-number edge) num))
            edges)
      #f))

(define (msg-send! edge msg)
  (vector-set! edge 1 msg))
(define (msg-spam! edges-out msg)
  (for-each (lambda (edge)
              (msg-send! edge msg))
            edges-out))

;;; highlight edges of non-null message
(define (gui-edge-highlight-all gui edges color)
  (for-each (lambda (edge)
              (if (not (null? (edge-msg edge)))
                  (gui-edge-highlight gui edge color)))
            edges))

;;; show edge text of all non-null messages
(define (gui-edge-text-all gui edges)
  (for-each (lambda (edge)
              (if (not (null? (edge-msg edge)))
                  (gui-edge-text gui edge (edge-msg edge))))
            edges))

