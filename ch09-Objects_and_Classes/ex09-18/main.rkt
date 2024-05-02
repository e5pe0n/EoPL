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
  class c1 extends object
    method private initialize() 1
    method public m1() send self m2()
    method public m2() 13

  class c2 extends c1
    method public m1() 22
    method public m2() 23
    method public m3() super m1()

  class c3 extends c2
    method public m1() 32
    method public m2() 33

  let o3 = new c3()
  in send o3 m3()
"))
; #(struct:num-val 13)
