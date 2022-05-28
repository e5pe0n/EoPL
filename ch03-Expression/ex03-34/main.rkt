#lang eopl

(require "lang.rkt")
(require "interp.rkt")
(require "utils.rkt")

; String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))
  )
)

(print
  (run "
    letrec double (x)
      = if zero? (x) then 0 else -((double -(x, 1)), -2)
    in (double 6)
  ")
) ; #(struct:num-val 12)