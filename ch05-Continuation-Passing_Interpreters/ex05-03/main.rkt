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
  -(-(44, 11), 3)
"))
; End of computation.
; #(struct:num-val 30)

(eopl:printf "~s~%" (run "
  let2 a = 44 b = 11
  in -(a, b)
"))
; End of computation.
; #(struct:num-val 33)
