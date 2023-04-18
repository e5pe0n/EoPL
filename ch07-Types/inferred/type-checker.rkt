#lang eopl

(require "utils.rkt")
(require "type.rkt")
(require "lang.rkt")

(define type-check!
  (lambda (string)
    (type-to-external-form
      (type-of-program (scan&parse string))
    )
  )
)

(println (type-check! "
  proc(x: int) -(x, 1)
"))
; (int -> int)

(println (type-check! "
  letrec
    int double(x: int) = if zero?(x)
                          then 0
                          else -((double -(x, 1)), -2)
  in double
"))
; (int -> int)

(println (type-check! "
  proc(f: (bool -> int)) proc(n: int) (f zero?(n))
"))
; ((bool -> int) -> (int -> int))

(println (type-check! "
  letrec ? foo(x: ?) = if zero?(x)
                        then 1
                        else -(x, (foo -(x, 1)))
  in foo
"))
; (int -> int)
