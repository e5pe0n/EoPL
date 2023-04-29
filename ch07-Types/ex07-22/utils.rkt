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

; Var -> ()
(define report-no-binding-found
  (lambda (err-name search-var)
    (eopl:error err-name ": No binding for ~s" search-var)
  )
)

; (SchemeVal -> Bool) -> (SchemeVal -> Bool)
(define vector-of
  (lambda (pred)
    (lambda (x)
      (and (vector? x)
        (let ([len (vector-length x)])
          (let loop ([i 0])
            (if (= i len)
              #t
              (and (pred (vector-ref x i)) (loop (+ i 1)))
            )
          )
        )
      )
    )
  )
)
