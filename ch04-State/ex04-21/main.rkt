; ex04-17 - ex04-19
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
    let x = 11
    in let p = proc (y) -(y, x)
      in -(setdynamic x = 17 during (p 22), (p 13))
  ")
) ; #(struct:num-val 3)
