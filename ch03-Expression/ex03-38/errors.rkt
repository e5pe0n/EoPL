#lang eopl

(provide (all-defined-out))

; Var -> ()
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env ": No binding for ~s" search-var)
  )
)

; Var -> ()
(define report-unbound-var
  (lambda (search-var)
    (eopl:error 'apply-senv ": No binding for ~s" search-var)
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

; Exp -> ()
(define report-invalid-source-expression
  (lambda (exp1)
    (eopl:error 'translation-of "invalid expression: ~s" exp1)
  )
)

; Exp -> ()
(define report-invalid-translated-expression
  (lambda (exp1)
    (eopl:error 'value-of "invalid expression: ~s" exp1)
  )
)

; String -> ()
(define report-no-corresponding-condition
  (lambda (pname)
    (eopl:error pname ": No corresponding condition")
  )
)