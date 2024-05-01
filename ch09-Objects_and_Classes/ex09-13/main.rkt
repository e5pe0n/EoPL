#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "interp.rkt")

(define run
  (lambda (string)
    (let ([pgm (scan&parse string)])
      (value-of-program pgm)
    )
  )
)

(println (run "
  class oddeven extends object
    method private initialize() 1
    final method public even(n)
      if zero?(n) then 1 else send self odd(-(n, 1))
    final method public odd(n)
      if zero?(n) then 0 else send self even(-(n, 1))

  class bogus-oddeven extends oddeven
    method private initialize() 1
    method public even(n)
      1
    method public odd(n)
      0

  0
"))
; check-final-method!: class bogus-oddeven can't override method even in class oddeven.
