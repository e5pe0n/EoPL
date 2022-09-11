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

(eopl:printf "~s~%" (run "
  list(1, 2, 3)
"))
; End of Computation.
; #(struct:list-val (1 2 3))

(eopl:printf "~s~%" (run "
  let x = list(1, 2, 3)
  in car(x)
"))
; End of Computation.
; #(struct:num-val 1)

(eopl:printf "~s~%" (run "
  let x = list(1, 2, 3)
  in cdr(x)
"))
; End of Computation.
; #(struct:list-val (2 3))
