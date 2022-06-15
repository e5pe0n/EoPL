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
    in begin
      setref(deref(x), 10000);
      deref(deref(x))
      end
  ")
) ; #(struct:num-val 10000)
(print
  (run "
    let x = newref(22)
    in list (deref(x), setref(x, 10000), deref(x))
  ")
) ; #(struct:list-val (22 23 10000))
