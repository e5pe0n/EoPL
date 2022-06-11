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
    letrec
      even (x) = if zero? (x) then 1 else (odd -(x, 1))
      odd (x) = if zero? (x) then 0 else (even -(x, 1))
    in (odd 13)
  ")
) ; #(struct:num-val 1)
