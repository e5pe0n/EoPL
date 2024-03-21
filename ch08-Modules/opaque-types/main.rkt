#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "check-mods.rkt")
(require "interp.rkt")

; String -> ExpVal
(define run
  (lambda (string)
    (let ([pgm (scan&parse string)])
      (type-of-program pgm)
      (value-of-program pgm)
    )
  )
)

; (println (run "
;   module m1
;     interface [
;       opaque t
;       z: t
;       s: (t -> t)
;       is-z?: (t -> bool)
;     ]
;     body [
;       type t = int
;       z = 33
;       s = proc(x: t) -(x, -1)
;       is-z? = proc(x: t) zero?(-(x, z))
;     ]
;   proc(x: from m1 take t)
;     (from m1 take is-z? x)
; "))

; (println (run "
;   module ints1
;     interface [
;       opaque t
;       zero: t
;       succ: (t -> t)
;       pred: (t -> t)
;       is-zero: (t -> bool)
;     ]
;     body [
;       type t = int
;       zero = 0
;       succ = proc(x: t) -(x, -5)
;       pred = proc(x: t) -(x, 5)
;       is-zero = proc(x: t) zero?(x)
;     ]
;     let z = from ints1 take zero
;     in let s = from ints1 take succ
;       in (s (s z))
; "))
; ; #(struct:num-val 10)

; (println (run "
;   module ints1
;     interface [
;       opaque t
;       zero: t
;       succ: (t -> t)
;       pred: (t -> t)
;       is-zero: (t -> bool)
;     ]
;     body [
;       type t = int
;       zero = 0
;       succ = proc(x: t) -(x, -5)
;       pred = proc(x: t) -(x, 5)
;       is-zero = proc(x: t) zero?(x)
;     ]
;     let z = from ints1 take zero
;     in let s = from ints1 take succ
;     in let p = from ints1 take pred
;     in let z? = from ints1 take is-zero
;     in letrec
;       int to-int(x: from ints1 take t) = if (z? x)
;                                           then 0
;                                           else -((to-int (p x)), -1)
;     in (to-int (s (s z)))
; "))
; ; #(struct:num-val 2)

(println (run "
  module tables
    interface [
      opaque table
      empty: table
      add-to-table: (int -> (int -> (table -> table)))
      lookup-in-table: (int -> (table -> int))
    ]
    body [
      type table = (int -> int)
      empty = proc(x: int) 0
      add-to-table = proc(i: int) proc(v: int) proc(t: table)
                      proc(x: int)
                        if zero?(-(i, x)) then v else (t x)
      lookup-in-table = proc(i: int) proc(t: table) (t i)
    ]
    let empty = from tables take empty
    in let add-binding = from tables take add-to-table
    in let lookup = from tables take lookup-in-table
    in let table1 = (((add-binding 3) 300)
                      (((add-binding 4) 400)
                        (((add-binding 3) 600)
                          empty)))
    in -(((lookup 4) table1), ((lookup 3) table1))
"))
; #(struct:num-val 100)
