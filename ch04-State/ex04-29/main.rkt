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
    let a = newarray(2, -99)
    in let p = proc (x)
                let v = arrayref(x, 1)
                in arrayset(x, 1, -(v, -1))
    in begin
      arrayset(a, 1, 0);
      (p a);
      (p a);
      arrayref(a, 1)
    end
  ")
)
; #(struct:num-val 2)
