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
    let p = proc (x)
              set x = 4
    in let a = 3
      in begin
        (p a);
        a
      end
  ")
) ; #(struct:num-val 4)
(print
  (run "
    let swap = proc (x) proc (y)
                let tmp = x
                in begin
                  set x = y;
                  set y = tmp
                end
    in let a = 33
    in let b = 44
    in begin
      ((swap a) b);
      -(a, b)
    end
  ")
) ; #(struct:num-val 11)
(print
  (run "
    letrec
      even (x) = if zero? (x) then 1 else (odd -(x, 1))
      odd (x) = if zero? (x) then 0 else (even -(x, 1))
    in (odd 13)
  ")
) ; #(struct:num-val 1)
