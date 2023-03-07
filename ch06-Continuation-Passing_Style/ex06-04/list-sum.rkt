#lang eopl

(require "../utils.rkt")

; Listof(Int) -> Int
(define list-sum
  (lambda (loi)
    (if (null? loi)
      0
      (+ (car loi) (list-sum (cdr loi)))
    )
  )
)

(println (list-sum '(1 2 3))) ; 6
