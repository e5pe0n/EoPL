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
    method public getx()
      x

  class c2 extends c1

  let o2 = new c2()
  in list(
    letclass c2 = extends c1
                  method private initialize()
                    set x = 100
    in let lo2 = new c2()
    in send lo2 getx()
    ,
    send o2 getx()
  )
"))
; #(struct:list-val (#(struct:num-val 100) #(struct:num-val 0)))
