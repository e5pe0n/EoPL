#lang eopl

(require "lang.rkt")
(require "interp.rkt")
(require "utils.rkt")
(require "type.rkt")

; String -> ExpVal
(define run
  (lambda (string)
    (let ([pgm (scan&parse string)])
      (type-of-program pgm)
      (value-of-program pgm)
    )
  )
)

(println (run "
  let f = proc(x: int) -(x, 1)
  in (f 10)
"))
; #(struct:num-val 9)

(println (run "
  letrec
    int double(x: int) = if zero?(x)
                          then 0
                          else -((double -(x, 1)), -2)
  in (double 10)
"))
; #(struct:num-val 20)

(println (run "
  let g = proc(f: (bool -> int)) proc(n: int) (f zero?(n))
  in let h = proc(x: bool)
              if x
              then 0
              else 1
  in ((g h) 100)
"))
; #(struct:num-val 1)
