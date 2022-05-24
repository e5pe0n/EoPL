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
    let makemult = proc (maker)
      proc (x)
        if zero? (x)
          then 0
          else -(((maker maker) -(x, 1)), -4)
    in let times4 = proc (x) ((makemult makemult) x)
      in (times4 3)
  ")
) ; #(struct:num-val 12)
(print
  (run "
    let makemult = proc (maker)
      proc (x)
        proc (y)
          if zero? (x)
            then 0
            else -(
              (
                ((maker maker) -(x, 1))
              y),
              -(0, y)
            )
    in let times = proc (x) proc (y) (((makemult makemult) x) y)
      in ((times 3) 4)
  ")
) ; #(struct:num-val 12)