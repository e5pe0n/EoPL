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
    field private x
    method private initialize()
      set x = 0
    method private initialize(initx)
      set x = initx
    method public getx()
      x

  let o1 = new c1()
      o2 = new c1(100)
  in list(send o1 getx(), send o2 getx())
"))
; #(struct:list-val (#(struct:num-val 0) #(struct:num-val 100)))
