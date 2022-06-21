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
) ; #(struct:num-val 12)
(print
  (run "
    let f = proc () -(1, -2)
    in (f)
  ")
) ; #(struct:num-val 3)
(print
  (run "
    letrec
      even (x) = if zero? (x) then 1 else (odd -(x, 1))
      odd (x) = if zero? (x) then 0 else (even -(x, 1))
    in (odd 13)
  ")
) ; #(struct:num-val 1)
