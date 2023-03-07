#lang eopl

(require "utils.rkt")

(define fib
  (lambda (n)
    (fib/k n (end-cont))
  )
)

(define fib/k
  (lambda (n cont)
    (if (< n 2)
      (apply-cont cont 1)
      (fib/k (- n 1) (fib1-cont n cont))
    )
  )
)

(define end-cont
  (lambda ()
    (lambda (val) val)
  )
)

(define fib1-cont
  (lambda (n cont)
    (lambda (val1)
      (fib/k (- n 2) (fib2-cont val1 cont))
    )
  )
)

(define fib2-cont
  (lambda (val1 cont)
    (lambda (val2)
      (apply-cont cont (+ val1 val2))
    )
  )
)

(define apply-cont
  (lambda (cont val)
    (cont val)
  )
)

(println (fib 10))  ; 89
