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
    if less? ( - (100, 2), - (100, 1))
    then
      if greater? ( - (100, 2), - (100, 3))
      then
        if equal? ( -(100, 2), 98)
        then 0
        else 1
      else 2
    else 3
  ")
) ; #(struct:num-val 0)