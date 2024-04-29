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
  module double-ints-maker
    interface (
      (ints: [
        opaque t
        zero: t
        succ: (t -> t)
        pred: (t -> t)
        is-zero: (t -> bool)
      ]) => [
        opaque t
        zero: t
        succ: (t -> t)
        pred: (t -> t)
        is-zero: (t -> bool)
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
        type t = from ints take t
        zero = from ints take zero
        succ = proc(x: from ints take t)
                (from ints take succ (from ints take succ x))
        pred = proc(x: from ints take t)
                (from ints take pred (from ints take pred x))
        is-zero = from ints take is-zero
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
      succ = proc(x: t) -(x, -1)
      pred = proc(x: t) -(x, 1)
      is-zero = proc(x: t) zero?(x)
    ]

  module dints
    interface [
      opaque t
      zero: t
      succ: (t -> t)
      pred: (t -> t)
      is-zero: (t -> bool)
    ]
    body
      (double-ints-maker ints1)

  let z = from dints take zero
  in let s = from dints take succ
  in (s (s z))
"))
; #(struct:num-val 4)
