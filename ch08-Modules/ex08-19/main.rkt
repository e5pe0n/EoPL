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
  module from-int-maker
    interface (
      (ints: [
        opaque t
        zero: t
        succ: (t -> t)
        pred: (t -> t)
        is-zero: (t -> bool)
      ]) => [
        from-int: (int -> from ints take t)
      ]
    )
    body
      module-proc(ints: [
        opaque t
        zero: t
        succ: (t -> t)
        pred: (t -> t)
        is-zero: (t -> bool)
      ])
      [
        from-int = let z = from ints take zero
                    in let s = from ints take succ
                    in letrec from ints take t from-int(x: int) = if zero?(x)
                                                    then z
                                                    else (s (from-int -(x, 1)))
                    in from-int

      ]

  module ints1
    interface [
      opaque t
      zero: t
      succ: (t -> t)
      pred: (t -> t)
      is-zero: (t -> bool)
    ]
    body [
      type t = int
      zero = 0
      succ = proc(x: t) -(x, -5)
      pred = proc(x: t) -(x, 5)
      is-zero = proc(x: t) zero?(x)
    ]

  module ints1-from-int
    interface [
      from-int: (int -> from ints1 take t)
    ]
    body
      (from-int-maker ints1)

  (from ints1-from-int take from-int 2)
"))
; #(struct:num-val 10)
