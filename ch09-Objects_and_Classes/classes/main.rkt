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
