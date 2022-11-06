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
