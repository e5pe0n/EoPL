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
    let a = 3
    in let p = proc (x) -(x, a)
      in let a = 5
        in -(a, (p 2))
"))
; End of Computation.
; #(struct:num-val 8)

(println (run "
    let a = 3
    in let p = proc (z) a
    in let f = proc (x) (p 0)
    in let a = 5
    in (f 2)
"))
; End of Computation.
; #(struct:num-val 5)

(println (run "
    let a = 3
    in let p = proc (z) a
    in let f = proc (a) (p 0)
    in let a = 5
    in (f 2)
"))
; End of Computation.
; #(struct:num-val 2)
