#lang eopl

(require "lang.rkt")
(require "utils.rkt")

; String -> Bool
(define tf?
  (lambda (string)
    (tail-form? (scan&parse string))
  )
)

(define tail-form?
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (tail-form?-rec exp1)
      )
    )
  )
)


(define tail-form?-rec
  (lambda (exp)
    (cases inp-expression exp
      (const-exp (num) #t)
      (diff-exp (exp1 exp2)
        (and (simple-exp? exp1) (simple-exp? exp2))
      )
      (mul-exp (exp1 exp2)
        (and (simple-exp? exp1) (simple-exp? exp2))
      )
      (zero?-exp (exp1)
        (simple-exp? exp1)
      )
      (if-exp (exp1 exp2 exp3)
        (and (simple-exp? exp1)
          (tail-form?-rec exp2)
          (tail-form?-rec exp3)
        )
      )
      (var-exp (var) #t)
      (let-exp (vars exps body)
        (and
          (all vars identifier?)
          (all exps simple-exp?)
          (tail-form?-rec body)
        )
      )
      (letrec-exp (p-names b-varss p-bodies letrec-body)
        (begin
        (and
          (all p-names identifier?)
          (all b-varss (list-of identifier?))
          (all p-bodies tail-form?-rec)
          (tail-form?-rec letrec-body)
        )
        )
      )
      (proc-exp (vars exp1)
        (and
          (all vars identifier?)
          (tail-form?-rec exp1)
        )
      )
      (call-exp (rator rands)
        (and
          (simple-exp? rator)
          (all rands simple-exp?)
        )
      )

      (emptylist-exp () #t)
      (null?-exp (exp1)
        (simple-exp? exp1)
      )
      (number?-exp (exp1)
        (simple-exp? exp1)
      )
      (equal?-exp (exp1 exp2)
        (and (simple-exp? exp1) (simple-exp? exp2))
      )
      (less?-exp (exp1 exp2)
        (and (simple-exp? exp1) (simple-exp? exp2))
      )
      (greater?-exp (exp1 exp2)
        (and (simple-exp? exp1) (simple-exp? exp2))
      )
      (list-exp (exps)
        (all exps simple-exp?)
      )
      (cons-exp (exp1 exp2)
        (and (simple-exp? exp1) (simple-exp? exp2))
      )
      (car-exp (exp1)
        (simple-exp? exp1)
      )
      (cdr-exp (exp1)
        (simple-exp? exp1)
      )
      (add1-exp (exp1)
        (simple-exp? exp1)
      )
    )
  )
)

(define simple-exp?
  (lambda (exp)
    (cases inp-expression exp
      (let-exp (vars exps body) #f)
      (letrec-exp (p-names b-varss p-bodies letrec-body) #f)
      (if-exp (exp1 exp2 exp3) #f)
      (call-exp (rator rands) #f)
      (else tail-form?-rec exp)
    )
  )
)

; 1. removeall
; cps-in
(println (tf? "
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
")) ; #f
; cps-out
(println (tf? "
  letrec
    removeall(n, xs)
      = (removeall-k n xs proc(val) val)
    removeall-k(n, xs, cont)
      = if null?(xs)
        then (cont emptylist)
        else (removeall-k n cdr(xs) proc(val1)
          if number?(car(xs))
          then if equal?(n, car(xs))
                then (removeall-k n val1 cont)
                else (removeall-k n val1 proc(val2)
                        (cont cons(car(xs), val2))
                      )
          else (removeall-k n car(xs) proc(val2)
                  (removeall-k n cdr(xs) proc(val3)
                    (cont cons(val2, val3))
                  )
                )
        )
  in (removeall 1 list(1, 2, list(3, 1, 2), 3, list(2, list(1, 3))))
")) ; #t

; 2. occurs-in?
; cps-in
(println (tf? "
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
")) ; #f
; cps-out
(println (tf? "
  letrec
    occurs-in?(n, xs)
      = (occurs-in?-k n xs proc(val) val)
    occurs-in?-k(n, xs, cont)
      = if null?(xs)
        then (cont 0)
        else if number?(car(xs))
              then if equal?(n, car(xs))
                    then (cont 1)
                    else (occurs-in?-k n cdr(xs) cont)
              else (occurs-in?-k n car(xs) proc(val1)
                      if val1
                      then (cont 1)
                      else (occurs-in?-k n cdr(xs) cont)
                    )
  in (occurs-in? 6 list(1, 2, list(3, 4, 5), 6, list(7, list(8, 9))))
")) ; #t

; 3. remfirst
; cps-in
(println (tf? "
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
")) ; #f
; cps-out
(println (tf? "
  letrec
    occurs-in?-k(n, xs, cont)
      = if null?(xs)
        then (cont 0)
        else if number?(car(xs))
              then if equal?(n, car(xs))
                    then (cont 1)
                    else (occurs-in?-k n cdr(xs) cont)
              else (occurs-in?-k n car(xs) proc(val1)
                      if val1
                      then (cont 1)
                      else (occurs-in?-k n cdr(xs) cont)
                    )
    remfirst(n, xs)
      = (remfirst-k n xs proc(val) val)
    remfirst-k(n, xs, cont)
      = letrec loop-k(xs, cont)
                = if null?(xs)
                  then (cont emptylist)
                  else if number?(car(xs))
                        then if equal?(n, car(xs))
                              then (cont cdr(xs))
                              else (loop-k cdr(xs) proc(val1)
                                      (cont cons(car(xs), val1))
                                    )
                        else (occurs-in?-k n car(xs) proc(val1)
                                if val1
                                then (remfirst-k n car(xs) proc(val2)
                                        (cont cons(val2, cdr(xs)))
                                      )
                                else (remfirst-k n cdr(xs) proc(val2)
                                        (cont cons(car(xs), val2))
                                      )
                              )
        in (loop-k xs cont)
  in (remfirst 8 list(1, 2, list(3, 4, 5), 6, list(7, list(8, 9, 10), list(11, 12, 8)), 13, 8))
")) ; #t

; 4. depth
; cps-in
(println (tf? "
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
")) ; #f
; cps-out
(println (tf? "
  letrec depth(xs)
          = (depth-k xs proc(val) val)
        depth-k(xs, cont)
          = if null?(xs)
            then (cont 1)
            else if number?(car(xs))
                  then (depth-k cdr(xs) cont)
                  else (depth-k car(xs) proc(val1)
                          (depth-k cdr(xs) proc(val2)
                            if less?(add1(val1), val2)
                            then (cont val2)
                            else (cont add1(val1))
                          )
                        )
  in (depth list(1, 2, list(3, 4, 5), 6, list(7, list(8, 9, 10), list(11, 12, 8)), 13, 8))
")) ; #t

; 5. depth-with-let
; cps-in
(println (tf? "
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
")) ; #f
; cps-out
(println (tf? "
  letrec depth(xs)
          = (depth-k xs proc(val) val)
        depth-k(xs, cont)
          = if null?(xs)
            then (cont 1)
            else if number?(car(xs))
                  then (depth-k cdr(xs) cont)
                  else (depth-k car(xs) proc(val1)
                          (depth-k cdr(xs) proc(val2)
                            let dfirst = add1(val1)
                                drest = val2
                            in if less?(dfirst, drest)
                                then (cont drest)
                                else (cont dfirst)
                          )
                        )
  in (depth list(1, 2, list(3, 4, 5), 6, list(7, list(8, 9, 10), list(11, 12, 8)), 13, 8))
")) ; #t

; 6. map
; cps-in
(println (tf? "
  letrec map(f, xs)
          = if null?(xs)
            then emptylist
            else cons(
              (f car(xs)),
              (map f cdr(xs))
            )
        square(n) = *(n, n)
  in (map square list(1, 2, 3, 4, 5))
")) ; #f
; cps-out
(println (tf? "
  letrec map(f-k, xs)
          = (map-k f-k xs proc(val) val)
        map-k(f-k, xs, cont)
          = if null?(xs)
            then (cont emptylist)
            else (f-k car(xs) proc(val1)
                    (map-k f-k cdr(xs) proc(val2)
                      (cont cons(val1, val2))
                    )
                  )
        square-k(n, cont) = (cont *(n, n))
  in (map square-k list(1, 2, 3, 4, 5))
")) ; #t

; 7. fnlrgtn
; cps-in
(println (tf? "
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
")) ; #t
; cps-out
(println (tf? "
  letrec fnlrgtn(xs, n)
          = (fnlrgtn-k xs n proc(val) val)
        fnlrgtn-k(xs, n, cont)
          = if null?(xs)
            then (cont -1)
            else if number?(car(xs))
                  then if greater?(car(xs), n)
                        then (cont car(xs))
                        else (fnlrgtn-k cdr(xs) n cont)
                  else (fnlrgtn-k car(xs) n proc(val1)
                          if greater?(val1, -1)
                          then (cont val1)
                          else (fnlrgtn-k cdr(xs) n cont)
                        )
  in (fnlrgtn list(1, list(3, list(2), 7, list(9))) 6)
")) ; #t

; 8. every
; cps-in
(println (tf? "
  letrec every(pred, xs)
          = if null?(xs)
            then 1
            else if (pred car(xs))
                  then (every pred cdr(xs))
                  else 0
  in (every proc(n) greater?(n, 5) list(6, 7, 8, 9))
")) ; #f
; cps-out
(println (tf? "
  letrec every(pred-k, xs)
          = (every-k pred-k xs proc(val) val)
        every-k(pred-k, xs, cont)
          = if null?(xs)
            then 1
            else (pred-k car(xs) proc(val1)
                    if val1
                    then (every pred-k cdr(xs) cont)
                    else 0
                  )
  in (every proc(n cont) (cont greater?(n, 5)) list(6, 7, 8, 9))
")) ; #t
