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

; SchemeVal -> Void
(define print
  (lambda (x)
    (display x)
    (newline)
  )
)

; (SchemeVal -> Bool) -> (List -> Bool)
(define list-of
  (lambda (pred)
    (lambda (list1)
      (if (null? list1)
        #t
        (and
          (pred (car list1))
          (list-of pred) (cdr list1)
        )
      )
    )
  )
)

; List * SchemeVal -> Int
(define list-index
  (lambda (list1 x)
    (let f ([i 0] [lst1 list1])
      (if (null? lst1)
        -1
        (if (eqv? x (car lst1))
          i
          (f (+ 1 i) (cdr lst1))
        )
      )
    )
  )
)

; List * (SchemeVal -> Bool) -> List
(define filter
  (lambda (list1 pred)
    (let f ([lst1 list1])
      (if (null? lst1)
        '()
        (if (pred (car lst1))
          (cons (car lst1) (f (cdr lst1)))
          (f (cdr lst1))
        )
      )
    )
  )
)

