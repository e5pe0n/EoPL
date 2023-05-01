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

; ((T2 * T1) -> T2) * Listof(T1) * T2 -> T2
(define reduce
  (lambda (fn xs init-val)
    (if (null? xs)
      init-val
      (reduce fn (cdr xs) (fn init-val (car xs)))
    )
  )
)
