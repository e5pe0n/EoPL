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
  (run "-(-(x, 3), -(v, i))")
) ; #(struct:num-val 3)
(print
  (run "
    let x = 33
      in let y = 22
        in if zero? (-(x, 11)) then -(y, 2) else -(y, 4)
  ")
) ; #(struct:num-val 18)