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
    let x = - (100, 1)
    in cond
      equal? (x, 100) ==> 100
      equal? (x, 99) ==> 99
      equal? (x, 98) ==> 98
      end
  ")
) ; #(struct:num-val 99)