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

; ; 1. removeall
; (println (run "
;   letrec
;     removeall(n, xs)
;       = (removeall-k n xs proc(val) val)
;     removeall-k(n, xs, cont)
;       = if null?(xs)
;         then (cont emptylist)
;         else (removeall-k n cdr(xs) proc(val1)
;           let head = car(xs)
;           in if number?(head)
;               then if equal?(n, head)
;                     then (removeall-k n val1 cont)
;                     else (removeall-k n val1 proc(val2)
;                             (cont cons(head, val2))
;                           )
;               else (removeall-k n car(xs) proc(val2)
;                       (removeall-k n cdr(xs) proc(val3)
;                         (cont cons(val2, val3))
;                       )
;                     )
;               )
;   in let list1 = list(3, 1, 2)
;           list2 = list(1, 3)
;   in let list3 = list(2, list2)
;   in (removeall 1 list(1, 2, list1, 3, list3))
; "))
; ; "End of Computation."
; ; #(struct:list-val (2 (3 2) 3 (2 (3))))

; ; 2. occurs-in?
; (println (run "
;   letrec
;     occurs-in?(n, xs)
;       = (occurs-in?-k n xs proc(val) val)
;     occurs-in?-k(n, xs, cont)
;       = if null?(xs)
;         then (cont 0)
;         else
;           let head = car(xs)
;           in if number?(head)
;               then if equal?(n, head)
;                     then (cont 1)
;                     else (occurs-in?-k n cdr(xs) cont)
;               else (occurs-in?-k n car(xs) proc(val1)
;                       if val1
;                       then (cont 1)
;                       else (occurs-in?-k n cdr(xs) cont)
;                     )
;   in let list1 = list(3, 4, 5)
;           list2 = list(8, 9)
;   in let list3 = list(7, list2)
;   in (occurs-in? 6 list(1, 2, list1, 6, list3))
; "))
; ; "End of Computation."
; ; #(struct:num-val 1)

; ; 3. remfirst
; (println (run "
;   letrec
;     occurs-in?-k(n, xs, cont)
;       = if null?(xs)
;         then (cont 0)
;         else
;           let head = car(xs)
;           in if number?(head)
;               then if equal?(n, head)
;                     then (cont 1)
;                     else (occurs-in?-k n cdr(xs) cont)
;               else (occurs-in?-k n car(xs) proc(val1)
;                       if val1
;                       then (cont 1)
;                       else (occurs-in?-k n cdr(xs) cont)
;                     )
;     remfirst(n, xs)
;       = (remfirst-k n xs proc(val) val)
;     remfirst-k(n, xs, cont)
;       = letrec loop-k(xs, cont)
;                 = if null?(xs)
;                   then (cont emptylist)
;                   else
;                     let head = car(xs)
;                     in if number?(head)
;                         then if equal?(n, head)
;                               then (cont cdr(xs))
;                               else (loop-k cdr(xs) proc(val1)
;                                       (cont cons(head, val1))
;                                     )
;                         else (occurs-in?-k n car(xs) proc(val1)
;                                 if val1
;                                 then (remfirst-k n car(xs) proc(val2)
;                                         let tail = cdr(xs)
;                                         in (cont cons(val2, tail))
;                                       )
;                                 else (remfirst-k n cdr(xs) proc(val2)
;                                         (cont cons(head, val2))
;                                       )
;                               )
;         in (loop-k xs cont)
;   in let list1 = list(3, 4, 5)
;           list2 = list(8, 9, 10)
;           list3 = list(11, 12, 8)
;   in let list4 = list(7, list2, list3)
;   in (remfirst 8 list(1, 2, list1, 6, list4, 13, 8))
; "))
; ; "End of Computation."
; ; #(struct:list-val (1 2 (3 4 5) 6 (7 (9 10) (11 12 8)) 13 8))

; ; 4. depth
; (println (run "
;   letrec depth(xs)
;           = (depth-k xs proc(val) val)
;         depth-k(xs, cont)
;           = if null?(xs)
;             then (cont 1)
;             else
;               let head = car(xs)
;               in if number?(head)
;                   then (depth-k cdr(xs) cont)
;                   else (depth-k car(xs) proc(val1)
;                           (depth-k cdr(xs) proc(val2)
;                             let dfirst = add1(val1)
;                             in if less?(dfirst, val2)
;                             then (cont val2)
;                             else (cont dfirst)
;                           )
;                         )
;   in let list1 = list(3, 4, 5)
;           list2 = list(8, 9, 10)
;           list3 = list(11, 12, 8)
;   in let list4 = list(7, list2, list3)
;   in (depth list(1, 2, list1, 6, list4, 13, 8))
; "))
; ; "End of Computation."
; ; #(struct:num-val 3)

; ; 5. depth-with-let
; (println (run "
;   letrec depth(xs)
;           = (depth-k xs proc(val) val)
;         depth-k(xs, cont)
;           = if null?(xs)
;             then (cont 1)
;             else
;               let head = car(xs)
;               in if number?(head)
;                   then (depth-k cdr(xs) cont)
;                   else (depth-k car(xs) proc(val1)
;                           (depth-k cdr(xs) proc(val2)
;                             let dfirst = add1(val1)
;                                 drest = val2
;                             in if less?(dfirst, drest)
;                                 then (cont drest)
;                                 else (cont dfirst)
;                           )
;                         )
;   in let list1 = list(3, 4, 5)
;           list2 = list(8, 9, 10)
;           list3 = list(11, 12, 8)
;   in let list4 = list(7, list2, list3)
;   in (depth list(1, 2, list1, 6, list4, 13, 8))
; "))
; ; "End of Computation."
; ; #(struct:num-val 3)

; ; 6. map
; (println (run "
;   letrec map(f-k, xs)
;           = (map-k f-k xs proc(val) val)
;         map-k(f-k, xs, cont)
;           = if null?(xs)
;             then (cont emptylist)
;             else (f-k car(xs) proc(val1)
;                     (map-k f-k cdr(xs) proc(val2)
;                       (cont cons(val1, val2))
;                     )
;                   )
;         square-k(n, cont) = (cont *(n, n))
;   in (map square-k list(1, 2, 3, 4, 5))
; "))
; ; "End of Computation."
; ; #(struct:list-val (1 4 9 16 25))

; ; 7. fnlrgtn
; ;     suppose `xs` contains only positive integers
; (println (run "
;   letrec fnlrgtn(xs, n)
;           = (fnlrgtn-k xs n proc(val) val)
;         fnlrgtn-k(xs, n, cont)
;           = if null?(xs)
;             then (cont -1)
;             else
;               let head = car(xs)
;               in if number?(head)
;                   then if greater?(head, n)
;                         then (cont car(xs))
;                         else (fnlrgtn-k cdr(xs) n cont)
;                   else (fnlrgtn-k car(xs) n proc(val1)
;                           if greater?(val1, -1)
;                           then (cont val1)
;                           else (fnlrgtn-k cdr(xs) n cont)
;                         )
;   in let list1 = list(2)
;           list2 = list(9)
;   in let list3 = list(3, list1, 7, list2)
;   in (fnlrgtn list(1, list3) 6)
; "))
; ; "End of Computation."
; ; #(struct:num-val 7)

; 8. every
(println (run "
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
"))
; "End of Computation."
; #(struct:num-val 1)
