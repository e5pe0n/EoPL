#lang eopl

(require "lang.rkt")
(require "interp.rkt")
(require "utils.rkt")

; String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))
  )
)

(print
  (run "
    let a = newarray(10, 11, 12, 13, 14)
    in let b = newarray(20, 21, 22, 23, 24)
    in let swap = proc (x) proc (y)
                    let tmp = deref(x)
                    in begin
                      setref(x, deref(y));
                      setref(y, tmp)
                    end
    in let i = 1
    in let j = 2
    in begin
      ((swap arrayref(a, i)) arrayref(b, j));
      print(deref(arrayref(a, 0)));
      print(deref(arrayref(a, 1)));
      print(deref(arrayref(a, 2)));
      print(deref(arrayref(a, 3)));
      print(deref(arrayref(a, 4)));
      print(deref(arrayref(b, 0)));
      print(deref(arrayref(b, 1)));
      print(deref(arrayref(b, 2)));
      print(deref(arrayref(b, 3)));
      print(deref(arrayref(b, 4)))
    end
  ")
)
; #(struct:num-val 10)
; #(struct:num-val 22)
; #(struct:num-val 12)
; #(struct:num-val 13)
; #(struct:num-val 14)
; #(struct:num-val 20)
; #(struct:num-val 21)
; #(struct:num-val 11)
; #(struct:num-val 23)
; #(struct:num-val 24)
; #(struct:num-val 999)
