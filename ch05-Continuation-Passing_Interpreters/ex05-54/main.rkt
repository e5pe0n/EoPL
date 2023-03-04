#lang eopl

(require "lang.rkt")
(require "interp.rkt")
(require "utils.rkt")

; String -> ExpVal
(define run
  (lambda (string)
    (value-of-program 30 (scan&parse string))
  )
)

; (println (run "
;   letrec noisy (xs) = if null?(xs)
;                       then 0
;                       else begin
;                           print(car(xs));
;                           (noisy cdr(xs))
;                         end
;   in begin
;     spawn(proc (d) (noisy list(1, 2, 3, 4, 5)));
;     spawn(proc (d) (noisy list(6, 7, 8, 9, 10)));
;     print(100);
;     33
;   end
; "))
; ; ticks := 30
; ; #(struct:num-val 100)
; ; #(struct:num-val 1)
; ; #(struct:num-val 2)
; ; #(struct:num-val 6)
; ; #(struct:num-val 7)
; ; #(struct:num-val 3)
; ; #(struct:num-val 4)
; ; #(struct:num-val 5)
; ; #(struct:num-val 8)
; ; #(struct:num-val 9)
; ; #(struct:num-val 10)
; ; #(struct:num-val 33)

; (println (run "
;   let buffer = 0
;   in let producer = proc (n)
;                       letrec p-wait (k) = if zero?(k)
;                                         then set buffer = n
;                                         else begin
;                                             print(-(k, -200));
;                                             (p-wait -(k, 1))
;                                           end
;                       in (p-wait 5)
;   in let consumer = proc (d)
;                       letrec busywait (k) = if zero?(buffer)
;                                             then begin
;                                                 print(-(k, -100));
;                                                 (busywait -(k, -1))
;                                               end
;                                             else buffer
;                       in (busywait 0)
;   in begin
;     spawn(proc (d) (producer 44));
;     print(300);
;     (consumer 86)
;   end
; "))
; ; ticks := 30
; ; #(struct:num-val 300)
; ; #(struct:num-val 100)
; ; #(struct:num-val 101)
; ; #(struct:num-val 205)
; ; #(struct:num-val 204)
; ; #(struct:num-val 203)
; ; #(struct:num-val 102)
; ; #(struct:num-val 103)
; ; #(struct:num-val 104)
; ; #(struct:num-val 202)
; ; #(struct:num-val 201)
; ; #(struct:num-val 44)

; (println (run "
;   let x = 0
;   in let mut = mutex()
;   in let iner_x = proc (id) proc (dummy)
;                     begin
;                       wait(mut);
;                       set x = -(x, -1);
;                       print(x);
;                       signal(mut)
;                     end
;   in begin
;     spawn((iner_x 100));
;     spawn((iner_x 200));
;     spawn((iner_x 300))
;   end
; "))
; ; #(struct:num-val 1)
; ; #(struct:num-val 2)
; ; #(struct:num-val 3)
; ; #(struct:num-val 73)

; (println (run "
;   let f = proc (id) proc (dummy)
;               begin
;                 print(id);
;                 yield;
;                 print(id)
;               end
;   in begin
;     spawn((f 100));
;     spawn((f 200))
;   end
; "))
; ; #(struct:num-val 100)
; ; #(struct:num-val 200)
; ; #(struct:num-val 100)
; ; #(struct:num-val 200)
; ; #(struct:num-val 73)

; (println (run "
;   let f = proc(id)
;             begin
;               print(200);
;               print(id);
;               print(PARENT_TH_ID);
;               print(200)
;             end
;   in let g = proc(id)
;               begin
;                 print(100);
;                 print(id);
;                 print(PARENT_TH_ID);
;                 spawn(f);
;                 spawn(f);
;                 print(100)
;               end
;   in begin
;     print(spawn(g));
;     print(spawn(g))
;   end
; "))
; ; 1
; ; 2
; ; #(struct:num-val 100)
; ; #(struct:num-val 1)
; ; #(struct:num-val 0)
; ; #(struct:num-val 100)
; ; #(struct:num-val 100)
; ; #(struct:num-val 2)
; ; #(struct:num-val 0)
; ; #(struct:num-val 100)
; ; #(struct:num-val 200)
; ; #(struct:num-val 3)
; ; #(struct:num-val 1)
; ; #(struct:num-val 200)
; ; #(struct:num-val 200)
; ; #(struct:num-val 4)
; ; #(struct:num-val 1)
; ; #(struct:num-val 200)
; ; #(struct:num-val 200)
; ; #(struct:num-val 5)
; ; #(struct:num-val 2)
; ; #(struct:num-val 200)
; ; #(struct:num-val 200)
; ; #(struct:num-val 6)
; ; #(struct:num-val 2)
; ; #(struct:num-val 200)
; ; #(struct:num-val 26)

(println (run "
  let mut = mutex()
  in let f = proc(id)
              begin
                print(id);
                yield;
                print(id)
              end
  in let g = proc(id)
              begin
                print(id);
                wait(mut);
                signal(mut);
                print(id)
              end
  in begin
    spawn(f);
    spawn(g);
    wait(mut);
    yield;
    print(kill(1));
    print(kill(2));
    print(kill(3))
  end
"))
; #(struct:num-val 1)
; #(struct:num-val 2)
; #(struct:bool-val #t)
; #(struct:bool-val #t)
; #(struct:bool-val #f)
; #(struct:num-val 26)
