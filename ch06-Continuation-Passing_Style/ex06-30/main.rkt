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

; 1. removeall
(println (run "
  letrec
    removeall(n, xs)
      = if null?(xs)
        then emptylist
        else if number?(car(xs))
              then if equal?(n, car(xs))
                    then (removeall n cdr(xs))
                    else cons(
                      car(xs),
                      (removeall n cdr(xs))
                    )
              else cons(
                (removeall n car(xs)),
                (removeall n cdr(xs))
              )
  in (removeall 1 list(1, 2, list(3, 1, 2), 3, list(2, list(1, 3))))
"))
; "End of Computation."
; #(struct:list-val (2 (3 2) 3 (2 (3))))

; 2. occurs-in?
(println (run "
  letrec
    occurs-in?(n, xs)
      = if null?(xs)
        then 0
        else if number?(car(xs))
              then if equal?(n, car(xs))
                    then 1
                    else (occurs-in? n cdr(xs))
              else if (occurs-in? n car(xs))
                    then 1
                    else (occurs-in? n cdr(xs))
  in (occurs-in? 6 list(1, 2, list(3, 4, 5), 6, list(7, list(8, 9))))
"))
; "End of Computation."
; #(struct:num-val 1)

; 3. remfirst
(println (run "
  letrec
    occurs-in?(n, xs)
      = if null?(xs)
        then 0
        else if number?(car(xs))
              then if equal?(n, car(xs))
                    then 1
                    else (occurs-in? n cdr(xs))
              else
                if (occurs-in? n car(xs))
                then 1
                else (occurs-in? n cdr(xs))
  in letrec
      remfirst(n, xs)
        = letrec loop(xs)
                  = if null?(xs)
                    then emptylist
                    else if number?(car(xs))
                          then if equal?(n, car(xs))
                                then cdr(xs)
                                else cons(car(xs), (loop cdr(xs)))
                          else if (occurs-in? n car(xs))
                                then cons(
                                      (remfirst n car(xs)),
                                      cdr(xs)
                                    )
                                else cons(
                                      car(xs),
                                      (remfirst n cdr(xs))
                                    )
          in (loop xs)
  in (remfirst 8 list(1, 2, list(3, 4, 5), 6, list(7, list(8, 9, 10), list(11, 12, 8)), 13, 8))
"))
; "End of Computation."
; #(struct:list-val (1 2 (3 4 5) 6 (7 (9 10) (11 12 8)) 13 8))

; 4. depth
(println (run "
  letrec depth(xs)
          = if null?(xs)
            then 1
            else if number?(car(xs))
                  then (depth cdr(xs))
                  else if less?(
                            add1((depth car(xs))),
                            (depth cdr(xs))
                          )
                        then (depth cdr(xs))
                        else add1((depth car(xs)))
  in (depth list(1, 2, list(3, 4, 5), 6, list(7, list(8, 9, 10), list(11, 12, 8)), 13, 8))
"))
; "End of Computation."
; #(struct:num-val 3)

; 5. depth-with-let
(println (run "
  letrec depth(xs)
          = if null?(xs)
            then 1
            else if number?(car(xs))
                  then (depth cdr(xs))
                  else let dfirst = add1((depth car(xs)))
                            drest = (depth cdr(xs))
                        in if less?(dfirst, drest)
                            then drest
                            else dfirst
  in (depth list(1, 2, list(3, 4, 5), 6, list(7, list(8, 9, 10), list(11, 12, 8)), 13, 8))
"))
; "End of Computation."
; #(struct:num-val 3)

; 6. map
(println (run "
  letrec map(f, xs)
          = if null?(xs)
            then emptylist
            else cons(
              (f car(xs)),
              (map f cdr(xs))
            )
        square(n) = *(n, n)
  in (map square list(1, 2, 3, 4, 5))
"))
; "End of Computation."
; #(struct:list-val (1 4 9 16 25))

; 7. fnlrgtn
;     suppose `xs` contains only positive integers
(println (run "
  letrec fnlrgtn(xs, n)
          = if null?(xs)
            then -1
            else if number?(car(xs))
                  then if greater?(car(xs), n)
                        then car(xs)
                        else (fnlrgtn cdr(xs) n)
                  else let res = (fnlrgtn car(xs) n)
                        in if greater?(res, -1)
                            then res
                            else (fnlrgtn cdr(xs) n)
  in (fnlrgtn list(1, list(3, list(2), 7, list(9))) 6)
"))
; "End of Computation."
; #(struct:num-val 7)

; 8. every
(println (run "
  letrec every(pred, xs)
          = if null?(xs)
            then 1
            else if (pred car(xs))
                  then (every pred cdr(xs))
                  else 0
  in (every proc(n) greater?(n, 5) list(6, 7, 8, 9))
"))
; "End of Computation."
; #(struct:num-val 1)
