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
  module equality-maker
    interface (
      (ints: [
        opaque t
        zero: t
        succ: (t -> t)
        pred: (t -> t)
        is-zero: (t -> bool)
      ]) => [
        equal: (from ints take t -> (from ints take t -> bool))
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
        equal = let z? = from ints take is-zero
                in let p = from ints take pred
                in letrec (from ints take t -> bool) equal(x: from ints take t) =
                  proc(y: from ints take t)
                    if (z? x)
                    then (z? y)
                    else ((equal (p x)) (p y))
                in equal
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

  module equality
    interface [
      equal: (from ints1 take t -> (from ints1 take t -> bool))
    ]
    body
      (equality-maker ints1)

  let z = from ints1 take zero
  in let s = from ints1 take succ
  in let eq = from equality take equal
  % in ((eq (s (s z))) (s z))
  in ((eq (s (s z))) (s (s z)))
"))
; #(struct:bool-val #f)
; #(struct:bool-val #t)
