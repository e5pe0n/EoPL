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

(println (run "
  letrec fact-iter-acc (n, a)
            = if zero?(n)
                then a
                else (fact-iter-acc -(n, 1) *(n, a))
  in let fact-iter = proc (n) (fact-iter-acc n 1)
  in (fact-iter 4)
"))
; max-cont-size=3

(println (run "
  letrec fact-iter-acc (n, a)
            = if zero?(n)
                then a
                else (fact-iter-acc -(n, 1) *(n, a))
  in let fact-iter = proc (n) (fact-iter-acc n 1)
  in (fact-iter 5)
"))
; max-cont-size=3

(println (run "
  letrec fact-iter-acc (n, a)
            = if zero?(n)
                then a
                else (fact-iter-acc -(n, 1) *(n, a))
  in let fact-iter = proc (n) (fact-iter-acc n 1)
  in (fact-iter 6)
"))
; max-cont-size=3

; max-cont-size is a constant
