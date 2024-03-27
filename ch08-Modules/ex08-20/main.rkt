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
  module sum-prod-maker
    interface (
      (ints: [
        opaque t
        zero: t
        succ: (t -> t)
        pred: (t -> t)
        is-zero: (t -> bool)
      ]) => [
        plus: (from ints take t -> (from ints take t -> from ints take t))
        times: (from ints take t -> (from ints take t -> from ints take t))
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
        plus = let z? = from ints take is-zero
                in let s = from ints take succ
                in let p = from ints take pred
                in letrec (from ints take t -> from ints take t) plus(x: from ints take t) =
                    proc(y: from ints take t)
                      if (z? y)
                      then x
                      else ((plus (s x)) (p y))
                in plus
        times = let z? = from ints take is-zero
                in let s = from ints take succ
                in let p = from ints take pred
                in letrec (from ints take t -> from ints take t) times(x: from ints take t) =
                    proc(y: from ints take t)
                      if (z? y)
                      then x
                      else ((times ((plus x) x)) (p y))
                in times
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

  module ints1-sum-prod
    interface [
      plus: (from ints1 take t -> (from ints1 take t -> from ints1 take t))
      times: (from ints1 take t -> (from ints1 take t -> from ints1 take t))
    ]
    body
      (sum-prod-maker ints1)

  let z = from ints1 take zero
  in let s = from ints1 take succ
  in let plus1 = from ints1-sum-prod take plus
  in let times1 = from ints1-sum-prod take times
  in ((plus1 ((times1 (s (s (s z)))) (s (s z)))) (s z))
"))
; #(struct:num-val 65)
