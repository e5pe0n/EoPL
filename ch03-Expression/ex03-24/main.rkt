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
    let oddmaker = proc (maker1)
      proc (maker2)
        proc (x)
          if zero? (x)
            then 0
            else (((maker2 maker1) maker2) -(x, 1))
      in let evenmaker = proc (maker1)
        proc (maker2)
          proc (x)
            if zero? (x)
              then 1
              else (((maker1 maker1) maker2) -(x, 1))
        in let odd = proc (x) (((oddmaker oddmaker) evenmaker) x)
          in (odd 13)
  ")
) ; #(struct:num-val 1)
(print
  (run "
    let oddmaker = proc (maker1)
      proc (maker2)
        proc (x)
          if zero? (x)
            then 0
            else (((maker2 maker1) maker2) -(x, 1))
      in let evenmaker = proc (maker1)
        proc (maker2)
          proc (x)
            if zero? (x)
              then 1
              else (((maker1 maker1) maker2) -(x, 1))
        in let odd = proc (x) (((oddmaker oddmaker) evenmaker) x)
          in (odd 14)
  ")
) ; #(struct:num-val 0)
(print
  (run "
    let oddmaker = proc (maker1)
      proc (maker2)
        proc (x)
          if zero? (x)
            then 0
            else (((maker2 maker1) maker2) -(x, 1))
      in let evenmaker = proc (maker1)
        proc (maker2)
          proc (x)
            if zero? (x)
              then 1
              else (((maker1 maker1) maker2) -(x, 1))
        in let even = proc (x) (((evenmaker oddmaker) evenmaker) x)
          in (even 13)
  ")
) ; #(struct:num-val 0)
(print
  (run "
    let oddmaker = proc (maker1)
      proc (maker2)
        proc (x)
          if zero? (x)
            then 0
            else (((maker2 maker1) maker2) -(x, 1))
      in let evenmaker = proc (maker1)
        proc (maker2)
          proc (x)
            if zero? (x)
              then 1
              else (((maker1 maker1) maker2) -(x, 1))
        in let even = proc (x) (((evenmaker oddmaker) evenmaker) x)
          in (even 14)
  ")
) ; #(struct:num-val 1)