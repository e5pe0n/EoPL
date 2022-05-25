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
    let makerec = proc (f)
      let d = proc (x)
                proc (z) ((f (x x)) z)
      in proc (n) ((f (d d)) n)
    in let maketimes4 = proc (f)
                          proc (x)
                            if zero? (x)
                              then 0
                              else -((f -(x, 1)), -4)
      in let times4 = (makerec maketimes4)
        in (times4 3)
 ")
) ; #(struct:num-val 12)