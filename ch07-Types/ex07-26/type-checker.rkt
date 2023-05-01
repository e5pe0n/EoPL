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

; (println (type-check! "
;   proc(x: int) -(x, 1)
; "))
; ; (int -> int)

; (println (type-check! "
;   letrec
;     int double(x: int) = if zero?(x)
;                           then 0
;                           else -((double -(x, 1)), -2)
;   in double
; "))
; ; (int -> int)

; (println (type-check! "
;   proc(f: (bool -> int)) proc(n: int) (f zero?(n))
; "))
; ; ((bool -> int) -> (int -> int))

; (println (type-check! "
;   letrec ? foo(x: ?) = if zero?(x)
;                         then 1
;                         else -(x, (foo -(x, 1)))
;   in foo
; "))
; ; (int -> int)

; (println (type-check! "
;   letrec ? even(odd: ?) = proc(x: ?) if zero?(x) then 1 else (odd -(x, 1))
;   in letrec ? odd(x: ?) = if zero?(x) then 0 else ((even odd) -(x, 1))
;   in (odd 13)
; "))
; ; int

; (println (type-check! "
;   letrec ? f(x: ?) = (f x)
;   in f
; "))
; ; (tvar0 -> tvar1)

; (println (type-check! "
;   letrec ? f(x: ?) = (f x)
;   in proc(n: ?) (f -(n, 1))
; "))
; ; (int -> tvar0)

; (println (type-check! "
;   newpair(1, zero?(1))
; "))
; ; (pairof int bool)

; (println (type-check! "
;   let p = newpair(1, 2)
;   in unpair x y = p
;   in -(x, y)
; "))
; ; int

; (println (type-check! "
;   unpair x y = 1
;   in x
; "))
; ; unification-failure: ty1=#(struct:int-type), ty2=#(struct:pair-type #(struct:tvar-type 0) #(struct:tvar-type 1)), exp=#(struct:unpair-exp x y #(struct:const-exp 1) #(struct:var-exp x))

; (println (type-check! "
;   unpair x y = newpair(1, 2)
;   in if x then 100 else 200
; "))
; ; unification-failure: ty1=#(struct:int-type), ty2=#(struct:bool-type), exp=#(struct:var-exp x)

; (println (type-check! "
;   proc(x: int, y: ?) -(x, y)
; "))
; ; ((int * int) -> int)

; (println (type-check! "
;   let f = proc(x: int, y: ?) -(x, y)
;   in (f 1 2)
; "))
; ; int

; (println (type-check! "
;   let g = proc(f: ?, n: ?) (f zero?(n) 200)
;       h = proc(x: bool, y: int)
;             if x
;             then y
;             else 0
;   in g
; "))
; ; ((((bool * int) -> tvar0) * int) -> tvar0)

; (println (type-check! "
;   let g = proc(f: ?, n: ?) (f zero?(n) 200)
;       h = proc(x: bool, y: int)
;             if x
;             then y
;             else 0
;   in (g h 300)
; "))
; ; int

; (println (type-check! "
;   letrec
;     ? even(x: int) = if zero?(x)
;                         then 1
;                         else (odd -(x, 1))
;     int odd(x: ?) = if zero?(x)
;                         then 0
;                         else (even -(x, 1))
;   in odd
; "))
; ; ((int) -> int)

; (println (type-check! "
;   list(1, 2, 3)
; "))
; ; (listof int)

; (println (type-check! "
;   list(
;     list(1, 2, 3),
;     list(1, 2, 3),
;     list(1, 2, 3)
;   )
; "))
; ; (listof (listof int))

; (println (type-check! "
;   list(1, zero?(2), 3)
; "))
; ; unification-failure: ty1=#(struct:bool-type), ty2=#(struct:int-type), exp=#(struct:list-exp #(struct:const-exp 1) (#(struct:zero?-exp #(struct:const-exp 2)) #(struct:const-exp 3)))

; (println (type-check! "
;   emptylist
; "))
; ; (listof tvar0)

; (println (type-check! "
;   cons(1, cons(2, emptylist))
; "))
; ; (listof int)

; (println (type-check! "
;   cons(zero?(1), cons(2, emptylist))
; "))
; ; unification-failure: ty1=#(struct:bool-type), ty2=#(struct:int-type), exp=#(struct:cons-exp #(struct:zero?-exp #(struct:const-exp 1)) #(struct:cons-exp #(struct:const-exp 2) #(struct:emptylist-exp)))

; (println (type-check! "
;   car(list(1, 2, 3))
; "))
; ; int

; (println (type-check! "
;   if car(list(1, 2, 3))
;   then 100
;   else 200
; "))
; ; unification-failure: ty1=#(struct:int-type), ty2=#(struct:bool-type), exp=#(struct:car-exp #(struct:list-exp #(struct:const-exp 1) (#(struct:const-exp 2) #(struct:const-exp 3))))

; (println (type-check! "
;   cdr(list(1, 2, 3))
; "))
; ; (listof int)

; (println (type-check! "
;   let xs = cdr(list(1, 2, 3))
;   in cons(zero?(0), xs)
; "))
; ; unification-failure: ty1=#(struct:bool-type), ty2=#(struct:int-type), exp=#(struct:cons-exp #(struct:zero?-exp #(struct:const-exp 0)) #(struct:var-exp xs))

(println (type-check! "
  newref(22)
"))
; (refto int)

(println (type-check! "
  deref(newref(22))
"))
; int

(println (type-check! "
  let ref = newref(22)
  in setref(ref, 100)
"))
; void

(println (type-check! "
  let ref = newref(22)
  in let x = deref(ref)
  in begin
    setref(ref, 1);
    -(x, deref(ref))
  end
"))
; int
