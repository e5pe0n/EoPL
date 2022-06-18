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
    let x = newref(22)
    in let f = proc (z)
        let zz = newref(-(z, deref(x)))
        in deref(zz)
      in -((f 66), (f 55))
  ")
) ; #(struct:num-val 11)
(print
  (run "
    let x = newref(newref(0))
    in setref(deref(x), 10000)
  ")
) ; #(struct:num-val 23)
