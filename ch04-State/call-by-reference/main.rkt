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
