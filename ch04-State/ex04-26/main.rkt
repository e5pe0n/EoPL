#lang eopl

(require "lang.rkt")
(require "interp.rkt")
(require "utils.rkt")

; String -> ExpVal
(define run
  (lambda (string)
    (result-of-program (scan&parse string))
  )
)

(run "
  var even, odd =
    proc (x)
      if zero? (x) then 1
      else (odd -(x, 1)),
    proc (x)
      if zero? (x) then 0
      else (even -(x, 1))
    ; {
    print (even 13);
    print (even 12);
    print (odd 13);
    print (odd 12)
  }
")

; #(struct:num-val 0)
; #(struct:num-val 1)
; #(struct:num-val 1)
; #(struct:num-val 0)
