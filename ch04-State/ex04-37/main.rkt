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
    let x = 1
    in let p = proc (x) 100
    in begin
      (p x);
      x
    end
  ")
) ; #(struct:num-val 100)
