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
  (lambda x
    (for-each (lambda (y) (display y) (display " ")) x)
    (newline)
  )
)