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
    letrec
      even (x) = if zero? (x) then 1 else (odd -(x, 1))
      odd (x) = if zero? (x) then 0 else (even -(x, 1))
    in (odd 13)
  ")
) ; #(struct:num-val 1)
(print
  (run "
    let add1 = proc (n) -(n, -1)
    in let fact = proc (n) (add1 n)
      in let fact = proc (n)
          if zero? (n)
          then 1
          else *(n, (fact -(n, 1)))
        in (fact 5)
  ")
) ; #(struct:num-val 25)