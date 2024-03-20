#lang eopl

(require "utils.rkt")
(require "lang.rkt")

(provide (all-defined-out))

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (saved-var symbol?)
    (saved-val s-val?)
    (saved-env environment?)
  )
  (extend-env*
    (saved-vars (list-of symbol?))
    (saved-vals (list-of s-val?))
    (saved-env environment?)
  )
  (extend-env-rec
    (p-names (list-of identifier?))
    (b-varss (list-of (list-of identifier?)))
    (bodies (list-of expression?))
    (env environment?)
  )
)

; Env * Env
(define env->saved-env
  (lambda (env)
    (cases environment env
      (empty-env ()
        (eopl:error 'env->saved-env "env->saved-env called on empty-env")
      )
      (extend-env (saved-var saved-val saved-env) saved-env)
      (extend-env* (saved-vars saved-vals saved-env) saved-env)
      (extend-env-rec (p-names b-varss bodies saved-env) saved-env)
    )
  )
)

(define-datatype proc proc?
  (procedure
    (vars (list-of identifier?))
    (body expression?)
    (saved-env environment?)
  )
)

(define-datatype typed-module typed-module?
  (simple-module (bindings environment?))
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
  (typed-module-val
    (typed-module1 typed-module?)
  )
)

; ExpVal -> Number
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else report-expval-extractor-error 'expval->num val)
    )
  )
)

; ExpVal -> Bool
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else report-expval-extractor-error 'expval->bool val)
    )
  )
)

; ExpVal -> Proc
(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc1) proc1)
      (else report-expval-extractor-error 'expval->proc val)
    )
  )
)

; ExpVal -> Proc
(define expval->typed-module
  (lambda (val)
    (cases expval val
      (typed-module-val (type-module1) type-module1)
      (else report-expval-extractor-error 'expval->typed-module val)
    )
  )
)

; String * ExpVal -> ()
(define report-expval-extractor-error
  (lambda (err-name val)
    (eopl:error err-name "Couldn't extract inside value; val=~s" val)
  )
)
