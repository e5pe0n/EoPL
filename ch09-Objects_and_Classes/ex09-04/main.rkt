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
    field i
    field j
    method initialize(x)
      begin
        set i = x;
        set j = -(0, x)
      end
    method countup(d)
      begin
        set i = +(i, d);
        set j = -(j, d)
      end
    method getstate()
      list(i, j)
  let t1 = 0
      t2 = 0
      o1 = new c1(3)
  in begin
    set t1 = send o1 getstate();
    send o1 countup(2);
    set t2 = send o1 getstate();
    list(t1, t2)
  end
"))
; #(struct:list-val (#(struct:list-val (#(struct:num-val 3) #(struct:num-val -3))) #(struct:list-val (#(struct:num-val 5) #(struct:num-val -5)))))

(println (run "
  class point extends object
    field x
    field y
    method initialize(initx, inity)
      begin
        set x = initx;
        set y = inity
      end
    method move(dx, dy)
      begin
        set x = +(x, dx);
        set y = +(y, dy)
      end
    method get-location() list(x, y)

  class colorpoint extends point
    field color
    method initialize(initx, inity, initcolor)
      begin
        set x = initx;
        set y = inity;
        set color = initcolor
      end
    method set-color (c) set color = c
    method get-color () color

  let o1 = new colorpoint(3, 4, 172)
  in begin
    print(send o1 get-color());
    print(send o1 get-location())
  end
"))
; #(struct:num-val 172)
; #(struct:list-val (#(struct:num-val 3) #(struct:num-val 4)))
; 29
