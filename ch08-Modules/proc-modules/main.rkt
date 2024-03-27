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
  module to-int-maker
    interface (
      (ints: [
        opaque t
        zero: t
        succ: (t -> t)
        pred: (t -> t)
        is-zero: (t -> bool)
      ]) => [
        to-int: (from ints take t -> int)
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
        to-int = let z? = from ints take is-zero
                  in let p = from ints take pred
                  in letrec int to-int(x: from ints take t) = if (z? x)
                                                              then 0
                                                              else -((to-int (p x)), -1)
                  in to-int
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

  module ints1-to-int
    interface [
      to-int: (from ints1 take t -> int)
    ]
    body
      (to-int-maker ints1)

  let two1 = (from ints1 take succ
              (from ints1 take succ
                from ints1 take zero))
  in (from ints1-to-int take to-int two1)
"))
; #(struct:num-val 2)

(println (run "
  module to-int-maker
    interface (
      (ints: [
        opaque t
        zero: t
        succ: (t -> t)
        pred: (t -> t)
        is-zero: (t -> bool)
      ]) => [
        to-int: (from ints take t -> int)
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
        to-int = let z? = from ints take is-zero
                  in let p = from ints take pred
                  in letrec int to-int(x: from ints take t) = if (z? x)
                                                              then 0
                                                              else -((to-int (p x)), -1)
                  in to-int
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

  module ints1-to-int
    interface [
      to-int: (from ints1 take t -> int)
    ]
    body
      (to-int-maker ints1)

  module ints2
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
      succ = proc(x: t) -(x, -3)
      pred = proc(x: t) -(x, 3)
      is-zero = proc(x: t) zero?(x)
    ]

  module ints2-to-int
    interface [
      to-int: (from ints2 take t -> int)
    ]
    body
      (to-int-maker ints2)

  let s1 = from ints1 take succ
  in let z1 = from ints1 take zero
  in let to-ints1 = from ints1-to-int take to-int

  in let s2 = from ints2 take succ
  in let z2 = from ints2 take zero
  in let to-ints2 = from ints2-to-int take to-int

  in let two1 = (s1 (s1 z1))
  in let two2 = (s2 (s2 z2))
  in -((to-ints1 two1), (to-ints2 two2))
"))
; #(struct:num-val 0)
