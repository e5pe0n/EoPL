#lang eopl

(provide (all-defined-out))

; Var -> ()
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'no-binding-error ": No binding for ~s" search-var)
  )
)

; Env -> ()
(define report-invalid-env
  (lambda (e)
    (eopl:error 'invalid-env-error ": Bad environment=~s" e)
  )
)

; String * ExpVal -> ()
(define report-expval-extractor-error
  (lambda (name val)
    (eopl:error name "invalid syntax: ~s" val)
  )
)

; SchemeVal -> ()
(define report-invalid-scheme-value-error
  (lambda (v)
    (eopl:error 'invalid-scheme-error "invalid scheme value: ~s" v)
  )
)

; () -> ()
(define report-uncaught-exception
  (lambda ()
    (eopl:error 'uncaught-exception-error "no excpetion handling")
  )
)

; InpExp -> ()
(define report-invalid-exp-to-cps-of-simple-exp
  (lambda (exp)
    (eopl:error 'invalid-exp-to-cps-of-simple-exp "invalid exp: ~s" exp)
  )
)
