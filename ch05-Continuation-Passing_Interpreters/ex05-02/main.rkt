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
  letrec fact-iter-acc (n) = proc (a)
                      if zero?(n)
                        then a
                        else ((fact-iter-acc -(n, 1)) *(n, a))
  in let fact = proc (n) ((fact-iter-acc n) 1)
  in (fact 4)
"))
; End of computation.
; #(struct:num-val 24)
