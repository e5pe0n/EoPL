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
    let times4 = 100
    in begin
        set times4 = proc (x)
                      if zero? (x)
                      then 0
                      else -((times4 -(x, 1)), -4);
        (times4 3)
      end
  ")
) ; #(struct:num-val 12)
