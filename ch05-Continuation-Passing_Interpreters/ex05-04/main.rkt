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
  let3 a = 44, b = 11, c = 12
  in -(-(a, b), c)
"))
; End of computation.
; #(struct:num-val 33)

(eopl:printf "~s~%" (run "
  let3 p1 = proc (x) -(x, 1),
        p2 = proc (x) -((p1 x), 1),
        p3 = proc (x) -((p2 x), 1)
  in (p3 10)
"))
; End of computation.
; #(struct:num-val 7)