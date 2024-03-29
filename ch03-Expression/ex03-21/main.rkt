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
    let f = proc (x, y) -(x, -(0, y))
    in (f 3 4)
  ")
) ; #(struct:num-val 7)
(print
  (run "
    let f = proc () -(1, -2)
    in (f)
  ")
) ; #(struct:num-val 3)