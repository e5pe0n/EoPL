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
  class counter extends object
    field cnt
    method initialize()
      set cnt = 0
    method incr()
      set cnt = +(cnt, 1)
    method cnt()
      cnt

  class queue extends object
    field xs
    field num-ops
    field cnter
    method initialize(new-cnter)
      begin
        set xs = list();
        set num-ops = 0;
        set cnter = new-cnter
      end
    method empty?()
      null?(xs)
    method enqueue(x)
      begin
        set xs = cons(x, xs);
        set num-ops = +(num-ops, 1);
        send cnter incr()
      end
    method dequeue()
      let head = 0
      in letrec loop(ys) = if null?(cdr(ys))
                        then begin
                          set head = car(ys);
                          emptylist
                        end
                        else cons(car(ys), (loop cdr(ys)))
      in begin
        set xs = (loop xs);
        set num-ops = +(num-ops, 1);
        send cnter incr();
        head
      end
    method num-ops()
      num-ops

  let cnter = new counter()
  in let q1 = new queue(cnter)
  in let q2 = new queue(cnter)
  in begin
    send q1 enqueue(10);
    send q1 enqueue(20);
    send q1 enqueue(30);
    send q1 dequeue();
    send q1 dequeue();
    send q1 dequeue();
    print(send q1 num-ops());

    send q2 enqueue(100);
    send q2 enqueue(200);
    print(send q2 num-ops());

    print(send cnter cnt())
  end
"))
; #(struct:num-val 6)
; #(struct:num-val 2)
; #(struct:num-val 8)
; 29
