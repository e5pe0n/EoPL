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

; Env * Var * Listof(ModuleDep) -> ExpVal
(define apply-env
  (lambda (env search-var m-deps)
    (let (
        [
          saved-val
          (cases environment env
            (empty-env () (report-no-binding-found 'apply-env search-var))
            (extend-env (saved-var saved-val saved-env)
              (if (eqv? search-var saved-var)
                saved-val
                (apply-env saved-env search-var m-deps)
              )
            )
            (extend-env* (saved-vars saved-vals saved-env)
              (let loop ([saved-vars saved-vars] [saved-vals saved-vals])
                (if (null? saved-vars)
                  (apply-env saved-env search-var m-deps)
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
                  (apply-env saved-env search-var m-deps)
                  (if (eqv? search-var (car p-names))
                    (proc-val (procedure (car b-varss) (car p-bodies) env))
                    (loop (cdr p-names) (cdr b-varss) (cdr p-bodies))
                  )
                )
              )
            )
          )
        ]
      )
      (cases expval saved-val
        (typed-module-val (_)
          (if (any (lambda (x) (eqv? x search-var)) m-deps)
            saved-val
            (report-module-not-found-error search-var m-deps)
          )
        )
        (else saved-val)
      )
    )
  )
)
