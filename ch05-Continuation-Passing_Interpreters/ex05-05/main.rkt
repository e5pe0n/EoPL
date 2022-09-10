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

(eopl:printf "~s~%" (run "
  -(-(44, 11), 3)
"))
; End of computation.
; #(struct:num-val 30)

(eopl:printf "~s~%" (run "
  cons(10, cons(3, emptylist))
"))
; End of computation.
; #(struct:num-val 33)

(eopl:printf "~s~%" (run "
  let x = 4
  in cons(
    x,
    cons(
      cons(-(x, 1), emptylist),
      emptylist
    )
  )
"))
; End of computation.
; #(struct:list-val (4 (3)))

(eopl:printf "~s~%" (run "
  null?(emptylist)
"))
; End of computation.
; #(struct:bool-val #t)

(eopl:printf "~s~%" (run "
  null?(cons(1, emptylist))
"))
; End of computation.
; #(struct:bool-val #f)

(eopl:printf "~s~%" (run "
  let x = cons(1, cons(2, cons(3, emptylist)))
  in car(x)
"))
; End of computation.
; #(struct:bool-val 1)

(eopl:printf "~s~%" (run "
  let x = cons(1, cons(2, cons(3, emptylist)))
  in cdr(x)
"))
; End of Computation.
; #(struct:list-val (2 3))
