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
    static next-serial-number = 1
    field private my-serial-number
    method public get-serial-number() my-serial-number
    method private initialize()
      begin
        set my-serial-number = next-serial-number;
        set next-serial-number = +(next-serial-number, 1)
      end

  let o1 = new c1()
      o2 = new c1()
  in list(send o1 get-serial-number(),
          send o2 get-serial-number())
"))
; #(struct:list-val (#(struct:num-val 1) #(struct:num-val 2)))
