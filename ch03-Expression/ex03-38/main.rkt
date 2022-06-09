#lang eopl

(require "lang.rkt")
(require "interp.rkt")
(require "translator.rkt")
(require "utils.rkt")

; String -> ExpVal
(define run
  (lambda (string)
    (value-of-program
      (translation-of-program
        (scan&parse string)
      )
    )
  )
)

(print
  (run "
    let x = - (100, 1)
    in cond
      zero? (-(x, 100)) ==> 100
      zero? (-(x, 99)) ==> 99
      zero? (-(x, 98)) ==> 98
      end
  ")
) ; #(struct:num-val 99)
