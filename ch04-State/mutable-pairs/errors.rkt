#lang eopl

(provide (all-defined-out))

; Var -> ()
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env ": No binding for ~s" search-var)
  )
)

; Env -> ()
(define report-invalid-env
  (lambda (e)
    (eopl:error 'apply-env ": Bad environment=~s" e)
  )
)

; String * ExpVal -> ()
(define report-expval-extractor-error
  (lambda (name val)
    (eopl:error name "invalid syntax: ~s" val)
  )
)

; Ref * Sto -> ()
(define report-invalid-reference
  (lambda (ref store)
    (eopl:error 'invalid-ref "invalid reference: ref=~s, store=~s" ref store)
  )
)
