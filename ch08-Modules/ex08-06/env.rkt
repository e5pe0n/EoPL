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
      (extend-env* (saved-vars saved-vals saved-env)
        (let loop ([saved-vars saved-vars] [saved-vals saved-vals])
          (if (null? saved-vars)
            (apply-env saved-env search-var)
            (if (eqv? search-var (car saved-vars))
              (car saved-vals)
              (loop (cdr saved-vars) (cdr saved-vals))
            )
          )
        )
      )
      (extend-env-rec (p-names b-varss p-bodies saved-env)
        (let loop ([p-names p-names] [b-varss b-varss] [p-bodies p-bodies])
          (if (null? p-names)
            (apply-env saved-env search-var)
            (if (eqv? search-var (car p-names))
              (proc-val (procedure (car b-varss) (car p-bodies) env))
              (loop (cdr p-names) (cdr b-varss) (cdr p-bodies))
            )
          )
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
      (extend-env-with-module (saved-m-name saved-m-val saved-env)
        (if (eqv? m-name saved-m-name)
          saved-m-val
          (lookup-module-name-in-env m-name saved-env)
        )
      )
      (else (lookup-module-name-in-env m-name (env->saved-env env)))
    )
  )
)
