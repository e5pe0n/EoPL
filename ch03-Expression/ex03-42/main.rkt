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
    let x = 200
    in let f = proc (z) -(z, x)
      in let x = 100
        in let g = proc (z) -(z, x)
          in -((f 1), (g 1))
  ")
) ; #(struct:num-val -100)
(print
  (run "
    letrec double (x) = if zero? (x) then 0 else -((double -(x, 1)), -2)
      in (double 6)
  ")
) ; #(struct:num-val 12)
