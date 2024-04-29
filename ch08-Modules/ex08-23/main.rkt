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
  module mybool
    interface [
      opaque t
      true: t
      false: t
      and: (t -> (t -> t))
      not: (t -> t)
      to-bool: (t -> bool)
      default: t
    ]
    body [
      type t = int
      true = 0
      false = 13
      and = proc(x: t) proc(y: t)
              if zero?(x)
              then y
              else false
      not = proc(x: t)
              if zero?(x)
              then false
              else true
      to-bool = proc(x: t) zero?(x)
      default = false
    ]

  module table-of
    interface (
      (m: [
        opaque t
        default: t
      ]) => [
        opaque table
        empty: table
        add-to-table: (int -> (from m take t -> (table -> table)))
        lookup-in-table: (int -> (table -> from m take t))
      ]
    )
    body
      module-proc(m: [
        opaque t
        default: t
      ])
      [
        type table = (int -> from m take t)
        empty = proc(x: int) from m take default
        add-to-table = proc(i: int) proc(v: from m take t) proc(t: table)
                        proc(x: int)
                          if zero?(-(i, x)) then v else (t x)
        lookup-in-table = proc(i: int) proc(t: table) (t i)
      ]

  module mybool-tables
    interface [
      opaque table
      empty: table
      add-to-table: (int -> (from mybool take t -> (table -> table)))
      lookup-in-table: (int -> (table -> from mybool take t))
    ]
    body
      (table-of mybool)

  let empty = from mybool-tables take empty
  in let add-binding = from mybool-tables take add-to-table
  in let lookup = from mybool-tables take lookup-in-table
  in let t = from mybool take true
  in let f = from mybool take false
  in let table1 = (((add-binding 3) t)
                    (((add-binding 4) f)
                      (((add-binding 3) f) empty)))
  in ((lookup 3) table1)
"))
; #(struct:num-val 0)
