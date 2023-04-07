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

; (Symbol | String) * List -> List
(define join
  (lambda (sep xs)
    (let f ([xs xs])
      (if (null? xs)
        '()
        (cons (car xs) (cons sep (f (cdr xs))))
      )
    )
  )
)

; SschemeVal -> Unspecified
(define report-invalid-scheme-val-error
  (lambda (x)
    (eopl:error 'invalid-scheme-val-error ": invalid value=~s" x)
  )
)

; (T -> Bool) * Listof(T) -> Int
(define list-index
  (lambda (pred xs)
    (let f ([i 0] [xs xs])
      (if (null? xs)
        #f
        (if (pred (car xs))
          i
          (f (+ i 1) (cdr xs))
        )
      )
    )
  )
)
