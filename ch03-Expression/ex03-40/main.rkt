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
    let x = 3
    in letrec double (x) =
        if zero? (x)
        then 0
        else -((double -(x, 1)), -2)
      in let y = 6
        in -((double x), y)
  ")
) ; #(struct:num-val 0)
