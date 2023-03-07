#lang eopl

(require "utils.rkt")

(define fact
  (lambda (n)
    (fact/k n (end-cont))
  )
)

(define fact/k
  (lambda (n cont)
    (if (zero? n)
      (apply-cont cont 1)
      (fact/k (- n 1) (fact1-cont n cont))
    )
  )
)

(define end-cont
  (lambda ()
    (lambda (val) val)
  )
)

(define fact1-cont
  (lambda (saved-n saved-cont)
    (lambda (val)
      (apply-cont saved-cont (* saved-n val))
    )
  )
)

(define apply-cont
  (lambda (cont val)
    (cont val)
  )
)

(println (fact 5))  ; 120
