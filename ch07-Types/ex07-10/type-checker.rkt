#lang eopl

(require "utils.rkt")
(require "type.rkt")
(require "lang.rkt")

(define type-check!
  (lambda (string)
    (type-of-program (scan&parse string))
  )
)


(println (type-to-external-form (type-check! "
  proc(x: int, y: int) -(x, y)
")))
; ((int int) -> int)

(println (type-to-external-form (type-check! "
  letrec
    int even(x: int) = if zero?(x)
                        then 1
                        else (odd -(x, 1))
    int odd(x: int) = if zero?(x)
                        then 0
                        else (even -(x, 1))
  in odd
")))
; ((int) -> int)

(println (type-to-external-form (type-check! "
  let g = proc(f: (bool * int -> int), n: int) (f zero?(n) 200)
      h = proc(x: bool, y: int)
            if x
            then y
            else 0
  in g
")))
; ((((bool int) -> int) int) -> int)

(println (type-to-external-form (type-check! "
  newref(22)
")))
; (refto int)

(println (type-to-external-form (type-check! "
  deref(newref(22))
")))
; int

(println (type-to-external-form (type-check! "
  setref(0, 10000)
")))
; void
