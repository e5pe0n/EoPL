#lang eopl

(provide (all-defined-out))

; Any -> Bool
(define s-val?
  (lambda (x)
    #t
  )
)

; SchemeVal -> Bool
(define identifier?
  (lambda (x)
    (symbol? x)
  )
)

; SchemeVal -> ()
(define println
  (lambda (x)
    (eopl:printf "~s~%" x)
  )
)

; Listof(T) * ((T) -> Bool) -> Listof(T)
(define filter
  (lambda (lst pred)
    (let f ([lst1 lst])
      (if (null? lst1)
        lst1
        (let ([head (car lst1)] [tail (cdr lst1)])
          (if (pred head)
            (cons head (f tail))
            (f tail)
          )
        )
      )
    )
  )
)

; List -> List
(define copy-list
  (lambda (lst)
    (let f ([lst1 lst])
      (if (null? lst1)
        lst1
        (cons (car lst1) (f (cdr lst1)))
      )
    )
  )
)

; Listof(T) * ((T) -> Bool) -> Bool
(define all
  (lambda (xs pred)
    (let loop ([xs1 xs])
      (if (null? xs1)
        #t
        (and (pred (car xs1)) (loop (cdr xs1)))
      )
    )
  )
)
