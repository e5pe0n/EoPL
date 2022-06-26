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
    letmutable x = 1
    in let y = 2
      in begin
        set x = 100;
        -(x, y)
      end
  ")
) ; #(struct:num-val 98)
(print
  (run "
    let x = 1
    in let y = 2
      in begin
        set y = 100;
        -(x, y)
      end
  ")
) ; invalid-assignment: invalid assignment: var=y is immutable.
