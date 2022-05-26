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
    letrec times (x y) =
      if zero? (x) then 0 else -((times -(x, 1) y), -(0, y))
    in (times 3 4)
  ")
) ; #(struct:num-val 12)