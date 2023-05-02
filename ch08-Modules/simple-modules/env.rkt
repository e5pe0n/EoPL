#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "data-structures.rkt")

(provide (all-defined-out))

; () -> Env
(define init-env
  (lambda ()
    (empty-env)
  )
)

; Env * Var -> ExpVal
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env () (report-no-binding-found 'apply-env search-var))
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)
        )
      )
      (extend-env-rec (p-name b-var p-body saved-env)
        (if (eqv? p-name search-var)
          (proc-val (procedure b-var p-body env))
          (apply-env saved-env search-var)
        )
      )
      (extend-env-with-module (m-name m-val saved-env)
        (apply-env saved-env search-var)
      )
    )
  )
)

; Sym * Sym * Env -> ExpVal
(define lookup-qualified-var-in-env
  (lambda (m-name var-name env)
    (let ([m-val (lookup-module-name-in-env m-name env)])
      (cases typed-module m-val
        (simple-module (bindings)
          (apply-env bindings var-name)
        )
      )
    )
  )
)

; Sym * Env -> TypedModule
(define lookup-module-name-in-env
  (lambda (m-name env)
    (cases environment env
      (empty-env ()
        (report-no-binding-found 'lookup-module-name-in-env m-name)
      )
      (extend-env (saved-var saved-val saved-env)
        (lookup-module-name-in-env m-name saved-env)
      )
      (extend-env-rec (p-name b-var body saved-env)
        (lookup-module-name-in-env m-name saved-env)
      )
      (extend-env-with-module (saved-m-name saved-m-val saved-env)
        (if (eqv? m-name saved-m-name)
          saved-m-val
          (lookup-module-name-in-env m-name saved-env)
        )
      )
    )
  )
)
