#lang eopl

(require "utils.rkt")
(require "errors.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; Env = Var -> SchemeVal

; () -> Env
(define empty-env
  (lambda ()
    (lambda (search-var)
      (report-no-binding-found search-var)
    )
  )
)

; Var * ExpVal * Env -> Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
        saved-val
        (apply-env saved-env search-var)
      )
    )
  )
)

; Env * Var -> ExpVal
(define apply-env
  (lambda (env search-var)
    (let ([res (env search-var)])
      (if (vector? res)
        (vector-ref res 0)
        res
      )
    )
  )
)

; SchemeVal -> Bool
(define proc?
  (lambda (val)
    (procedure? val)
  )
)


(define-datatype expval expval?
  (num-val
    (num number?)
  )
  (bool-val
    (bool boolean?)
  )
  (proc-val
    (proc1 proc?)
  )
  (vec-val
    (vec vector?)
  )
)

; ExpVal -> Number
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else report-expval-extractor-error 'num val)
    )
  )
)

; ExpVal -> Bool
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else report-expval-extractor-error 'bool val)
    )
  )
)

; ExpVal -> Proc
(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc1) proc1)
      (else report-expval-extractor-error 'proc val)
    )
  )
)

(define expval->vec
  (lambda (val)
    (cases expval val
      (vec-val (vec) vec)
      (else report-expval-extractor-error 'vec val)
    )
  )
)