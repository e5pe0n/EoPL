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
  -(-(44, 11), 3)
"))
; End of computation.
; #(struct:num-val 30)

(println (run "
  letrec fact-iter-acc (n) = proc (a)
                      if zero?(n)
                        then a
                        else ((fact-iter-acc -(n, 1)) *(n, a))
  in let fact = proc (n) ((fact-iter-acc n) 1)
  in (fact 4)
"))
; End of computation.
; #(struct:num-val 24)

(println (run "
  let p1 = proc (x, y) -(x, y)
  in (p1 10 1)
"))
; End of Computation.
; #(struct:num-val 9)

(println (run "
  let p1 = proc (x, y, z) -(-(x, y), z)
  in (p1 10 1 2)
"))
; End of Computation.
; #(struct:num-val 7)
