#lang eopl

(require "lang.rkt")
(require "interp.rkt")
(require "translator.rkt")
(require "utils.rkt")

; String -> ExpVal
(define run
  (lambda (string)
    (value-of-program
      (translation-of-program
        (scan&parse string)
      )
    )
  )
)

(print
  (run "
    let u = 7
    in unpack x y = cons (u, cons (3, emptylist))
      in - (x, y)
  ")
) ; #(struct:num-val 4)
