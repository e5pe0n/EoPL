#lang eopl

(require "utils.rkt")
(require "type.rkt")
(require "lang.rkt")

(define type-check!
  (lambda (string)
    (let ([ty (type-to-external-form (type-of-program (scan&parse string)))])
      (apply-subst-to-sexp (canonical-subst ty) ty)
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

(println (type-check! "
  letrec ? even(odd: ?) = proc(x: ?) if zero?(x) then 1 else (odd -(x, 1))
  in letrec ? odd(x: ?) = if zero?(x) then 0 else ((even odd) -(x, 1))
  in (odd 13)
"))
; int

(println (type-check! "
  letrec ? f(x: ?) = (f x)
  in f
"))
; (tvar0 -> tvar1)

(println (type-check! "
  letrec ? f(x: ?) = (f x)
  in proc(n: ?) (f -(n, 1))
"))
; (int -> tvar0)
