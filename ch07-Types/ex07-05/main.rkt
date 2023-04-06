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
  let f = proc(x: int, y: int) -(x, y)
  in (f 10 1)
"))
; #(struct:num-val 9)

(println (run "
  letrec
    int even(x: int) = if zero?(x)
                        then 1
                        else (odd -(x, 1))
    int odd(x: int) = if zero?(x)
                        then 0
                        else (even -(x, 1))
  in (odd 13)
"))
; #(struct:num-val 1)

(println (run "
  let g = proc(f: (bool * int -> int), n: int) (f zero?(n) 200)
      h = proc(x: bool, y: int)
            if x
            then y
            else 0
  in (g h 100)
"))
; #(struct:num-val 0)
