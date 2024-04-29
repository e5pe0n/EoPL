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
  class queue extends object
    field xs
    method initialize()
      set xs = list()
    method empty?()
      null?(xs)
    method enqueue(x)
      set xs = cons(x, xs)
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
        head
      end

  let q = new queue()
  in begin
    print(send q empty?());
    send q enqueue(10);
    print(send q empty?());
    send q enqueue(20);
    send q enqueue(30);
    print(send q dequeue());
    print(send q dequeue());
    print(send q dequeue());
    print(send q empty?())
  end
"))
; #(struct:bool-val #t)
; #(struct:bool-val #f)
; #(struct:num-val 10)
; #(struct:num-val 20)
; #(struct:num-val 30)
; #(struct:bool-val #t)
; 29
