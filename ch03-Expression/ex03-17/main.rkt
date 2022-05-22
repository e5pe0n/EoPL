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
    let x = 30
    in
      let* x = - (x, 1)
           y = - (x, 2)
      in - (x, y)
  ")
) ; #(struct:num-val 2)
