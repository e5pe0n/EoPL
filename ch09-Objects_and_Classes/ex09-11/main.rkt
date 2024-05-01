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
    field x
    field y
    method private initialize(initx)
      begin
        set x = initx;
        set y = -(0, initx)
      end
    method public setx(newx)
      begin
        set x = newx;
        send self sety(-(0, x))
      end
    method private sety(newy)
      set y = newy
    method protected protected-method()
      999

  class c2 extends c1
    method private initialize(initx)
      super initialize(initx)

  class c3 extends c2
    method private initialize(initx)
      super initialize(initx)
    method public call-protected-method()
      send self protected-method()

  let o2 = new c2(0)
      o3 = new c3(0)
  in begin
    send o2 setx(10);
    print(fieldref o2 x); % #(struct:num-val 10)
    print(fieldref o2 y); % #(struct:num-val -10)
    % send o2 sety(100);  % method-call-exp: method call not allowed: class c2, method sety.

    print(send o3 call-protected-method()) % #(struct:num-val 999)
    % send o3 protected-method()  % method-call-exp: method call not allowed: class c3, method protected-method.
  end
"))

