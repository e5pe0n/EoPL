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

(println (run "
  module tables
    interface [
      opaque table
      empty: table
      add-to-table: (int * int * table -> table)
      lookup-in-table: (int * table -> int)
    ]
    body [
      type table = (int -> int)
      empty = proc(x: int) 0
      add-to-table = proc(i: int, v: int, t: table)
                      proc(x: int)
                        if zero?(-(i, x)) then v else (t x)
      lookup-in-table = proc(i: int, t: table) (t i)
    ]
    let empty = from tables take empty
        add-binding = from tables take add-to-table
        lookup = from tables take lookup-in-table
    in let table1 = (add-binding 3 300 (add-binding 4 400 (add-binding 3 600 empty)))
    in -((lookup 4 table1), (lookup 3 table1))
"))
; #(struct:num-val 100)
