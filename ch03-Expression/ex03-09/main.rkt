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
    in cons (
      x,
      cons (
        cons (
          - (x, 1),
          emptylist
        ),
        emptylist
      )
    )
  ")
) ; #(struct:list-val (4 (3)))
(print
  (run "
    let x = cons (1, cons (2, emptylist))
    in if null? (cdr (cdr (x)))
        then 1
        else 0
  ")
) ; #(struct:num-val 1)