#lang eopl

(require "utils.rkt")
(require "type.rkt")
(require "lang.rkt")

(define type-check!
  (lambda (string)
    (type-of-program (scan&parse string))
  )
)


; (println (type-to-external-form (type-check! "
;   proc(x: int, y: int) -(x, y)
; ")))
; ; ((int int) -> int)

; (println (type-to-external-form (type-check! "
;   letrec
;     int even(x: int) = if zero?(x)
;                         then 1
;                         else (odd -(x, 1))
;     int odd(x: int) = if zero?(x)
;                         then 0
;                         else (even -(x, 1))
;   in odd
; ")))
; ; ((int) -> int)

; (println (type-to-external-form (type-check! "
;   let g = proc(f: (bool * int -> int), n: int) (f zero?(n) 200)
;       h = proc(x: bool, y: int)
;             if x
;             then y
;             else 0
;   in g
; ")))
; ; ((((bool int) -> int) int) -> int)

; (println (type-check! "
;   let x = 0
;   in begin
;     set x = 100;
;     set x = zero?(x)
;   end
; "))
; ; check-equal-type!: Types didn't match: int != bool in
; ; #(struct:assign-exp x #(struct:zero?-exp #(struct:var-exp x)))

; (println (type-check! "
;   let x = 0
;   in if x
;       then set x = zero?(x)
;       else set x = zero?(x)
; "))
; ; check-equal-type!: Types didn't match: int != bool in
; ; #(struct:var-exp x)

; (println (type-to-external-form (type-check! "
;   newpair(1, zero?(1))
; ")))
; ; (pairof int bool)

; (println (type-to-external-form (type-check! "
;   let p = newpair(1, 2)
;   in unpair x y = p
;   in -(x, y)
; ")))
; ; int

; (println (type-to-external-form (type-check! "
;   unpair x y = 1
;   in x
; ")))
; ; unpair-exp: expression must be a pair type;
; ;   received type=int
; ;   received expression=#(struct:const-exp 1)

(println (type-to-external-form (type-check! "
  list(1, 2, 3)
")))
; (listof int)

(println (type-to-external-form (type-check! "
  emptylist_ int
")))
; (listof int)

(println (type-to-external-form (type-check! "
  cons(1, cons(2, emptylist_ int))
")))
; (listof int)

(println (type-to-external-form (type-check! "
  car(list(1, 2, 3))
")))
; int

(println (type-to-external-form (type-check! "
  cdr(list(1, 2, 3))
")))
; (listof int)
