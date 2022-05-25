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
    let a = 3
    in let p = proc (x) -(x, a)
      in let a = 5
        in -(a, (p 2))
  ")
) ; #(struct:num-val 8)