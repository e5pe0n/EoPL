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
            (f tail)
            (cons head (f tail))
          )
        )
      )
    )
  )
)
