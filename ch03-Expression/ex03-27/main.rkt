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
    let makerec = traceproc (f)
                    let d = traceproc (x)
                              traceproc (z) ((f (x x)) z)
                    in traceproc (n) ((f (d d)) n)
    in let maketimes4 = traceproc (f)
                          traceproc (x)
                            if zero? (x)
                              then 0
                              else -((f -(x, 1)), -4)
      in let times4 = (makerec maketimes4)
        in (times4 3)
  ")
)

; enter
; exit
; enter
; enter
; exit
; enter
; exit
; enter
; enter
; enter
; exit
; enter
; exit
; enter
; enter
; enter
; exit
; enter
; exit
; enter
; enter
; enter
; exit
; enter
; exit
; enter
; exit
; exit
; exit
; exit
; exit
; exit
; exit
; exit
; #(struct:num-val 12)