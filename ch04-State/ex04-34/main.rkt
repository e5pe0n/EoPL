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
    let p = proc (x)
              set x = 4
    in let a = 3
    in letref set4 = p
    in begin
      (set4 a);
      a
    end
  ")
) ; #(struct:num-val 4)

; We can create aliases using `letref`
