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

(define-datatype continuation continuation?
  (end-cont)
  (fact1-cont
    (n integer?)
    (cont continuation?)
  )
)

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont () val)
      (fact1-cont (saved-n saved-cont)
        (apply-cont saved-cont (* saved-n val))
      )
    )
  )
)

(println (fact 5))  ; 120
