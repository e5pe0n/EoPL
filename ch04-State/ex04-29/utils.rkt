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

; Var * List -> Int | #f
(define location
  (lambda (x list1)
    (let f ([i 0] [lst1 list1])
      (if (null? lst1)
        #f
        (if (eqv? (car lst1) x)
          i
          (f (+ i 1) (cdr lst1))
        )
      )
    )
  )
)
