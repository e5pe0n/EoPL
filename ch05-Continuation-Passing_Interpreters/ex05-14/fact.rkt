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
  letrec fact (n) = if zero?(n) then 1 else *(n, (fact -(n, 1)))
  in (fact 4)
"))
; max-cont-size=7

(println (run "
  letrec fact (n) = if zero?(n) then 1 else *(n, (fact -(n, 1)))
  in (fact 5)
"))
; max-cont-size=8

(println (run "
  letrec fact (n) = if zero?(n) then 1 else *(n, (fact -(n, 1)))
  in (fact 6)
"))
; max-cont-size=9

; max-cont-size grows linealy with its argument
