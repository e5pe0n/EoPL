#lang eopl

(provide (all-defined-out))

; Var -> Void
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env ": No binding for ~s" search-var)
  )
)

; Env -> Void
(define report-invalid-env
  (lambda (e)
    (eopl:error 'apply-env ": Bad environment=~s" e)
  )
)

; String x ExpVal -> Void
(define report-expval-extractor-error
  (lambda (name val)
    (eopl:error name "invalid syntax: ~s" val)
  )
)