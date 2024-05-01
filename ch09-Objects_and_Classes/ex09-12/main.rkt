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
    field public y
    method private initialize(initx)
      begin
        set x = initx;
        set y = 0
      end
    method public setx(newx)
      set x = newx
    method public getx()
      x

  let o1 = new c1(0)
  in begin
    send o1 setx(100);
    print(send o1 getx());  % #(struct:num-val 100)
    % print(fieldref o1 x); % fieldref: field access not allowed: class c1, field x.

    fieldset o1 y = 111;
    print(fieldref o1 y)  % #(struct:num-val 111)
  end
"))

