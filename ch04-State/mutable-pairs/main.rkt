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
    let glo = pair(11, 22)
    in let f = proc (loc)
                let d1 = setright(loc, left(loc))
                in let d2 = setleft(glo, 99)
                in -(left(loc), right(loc))
    in (f glo)
  ")
)
; #(struct:num-val 88)
