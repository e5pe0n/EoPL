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
    let x = 4
    in list (x, - (x, 1), - (x, 3))
  ")
) ; #(struct:list-val (4 3 1))
(print
  (run "
    let x = list (1, 2)
    in if equal? (car (x), 1)
      then
        if null? (cdr (cdr (x)))
          then 2
          else 1
      else
        0
  ")
) ; #(struct:num-val 2)