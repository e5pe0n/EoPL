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

; (println (run "
;   let a = 44
;       b = 11
;       c = 12
;   in -(-(a, b), c)
; "))
; ; End of Computation.
; ; #(struct:num-val 21)

; (println (run "
;   let p1 = proc (x) -(x, 1)
;       p2 = proc (p) (p 10)
;   in (p2 p1)
; "))
; ; ; End of Computation.
; ; ; #(struct:num-val 9)

; (println (run "
;   let p1 = proc (x, y) -(x, y)
;   in (p1 10 1)
; "))
; ; End of Computation.
; ; #(struct:num-val 9)

; (println (run "
;   let p1 = proc (x, y, z) -(-(x, y), z)
;   in (p1 10 1 2)
; "))
; ; End of Computation.
; ; #(struct:num-val 7)

; (println (run "
;   let index = proc (n)
;                 letrec inner (lst)
;                   = if null? (lst)
;                     then raise 99
;                     else if zero? (-(car(lst), n))
;                           then 0
;                           else -((inner cdr(lst)), -1)
;                 in proc (lst)
;                     try (inner lst)
;                     catch (x) -1
;   in ((index 5) list(2, 3))
; "))
; ; ; End of Computation.
; ; ; #(struct:num-val -1)

; (println (run "
;   let index = proc (n)
;                 letrec inner (lst)
;                   = if null? (lst)
;                     then raise 99
;                     else if zero? (-(car(lst), n))
;                           then 0
;                           else -((inner cdr(lst)), -1)
;                 in proc (lst)
;                     try (inner lst)
;                     catch (x) -1
;   in ((index 5) list(2, 3, 5))
; "))
; ; ; End of Computation.
; ; ; #(struct:num-val 2)

; (println (run "
;   let f = proc (x, y) -(x, y)
;   in try (f 1 2 3)
;       catch (x) x
; "))
; ; ; End of Computation.
; ; ; #(struct:num-val 100)


; (println (run "
;   let x = 1
;       y = 0
;   in try / (x, y)
;       catch (x) x
; "))
; ; ; End of Computation.
; ; ; #(struct:num-val 101)

(println (run "
  call/cc (proc (k) -(5, (k 4)))
"))
; End of Computation.
; #(struct:num-val 4)

(println (run "
  call/cc (proc (k) -(5, 4))
"))
; End of Computation.
; #(struct:num-val 1)

(println (run "
  let product = proc (xs)
                  call/cc (proc (break)
                    letrec f (xs) = if null? (xs)
                                    then 1
                                    else if zero? (car (xs))
                                          then (break 0)
                                          else *(car (xs), (f cdr (xs)))
                    in (f xs)
                  )
  in (product list (1, 2, 3, 4, 5))
"))
; End of Computation.
; #(struct:num-val 120)

(println (run "
  let product = proc (xs)
                  call/cc (proc (break)
                    letrec f (xs) = if null? (xs)
                                    then 1
                                    else if zero? (car (xs))
                                          then (break 0)
                                          else *(car (xs), (f cdr (xs)))
                    in (f xs)
                  )
  in (product list (7, 3, 8, 0, 1, 9, 5))
"))
; End of Computation.
; #(struct:num-val 0)
