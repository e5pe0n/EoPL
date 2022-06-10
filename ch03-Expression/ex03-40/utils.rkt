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
