#lang eopl

(require "utils.rkt")

(define fact
  (lambda (n)
    (if (zero? n)
      1
      (* n (f (- n 1)))
    )
  )
)

(println (fact 5))  ; 120
