#lang eopl

(require "utils.rkt")

(define list-sum
  (lambda (loi)
    (list-sum/k loi (end-cont))
  )
)

(define list-sum/k
  (lambda (loi cont)
    (if (null? loi)
      (apply-cont cont 0)
      (list-sum/k (cdr loi) (list-sum-cont (car loi) cont))
    )
  )
)

(define end-cont
  (lambda ()
    0
  )
)

(define list-sum-cont
  (lambda (n cont)
    (+ cont n)
  )
)

(define apply-cont
  (lambda (cont val)
    (+ cont val)
  )
)

(println (list-sum '(1 2 3)))
; "End of Computation."
; "This sentense should appear only once."
; 6

