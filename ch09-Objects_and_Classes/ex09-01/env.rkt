#lang eopl

(require "data-structs.rkt")
(require "store.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(define init-env
  (lambda () (empty-env))
)
(define apply-env
  (lambda (env search-name)
    (cases environment env
      (empty-env ()
        (eopl:error 'apply-env "No binding for ~s" search-name)
      )
      (extend-env (b-vars b-vals saved-env)
        (cond
          ((position (lambda (x) (eqv? x search-name)) b-vars)
            => (lambda (n) (list-ref b-vals n))
          )
          (else  (apply-env saved-env search-name))
        )
      )
      (extend-env-rec** (p-names b-varss p-bodies saved-env)
        (cond
          ((position (lambda (x) (eqv? x search-name)) p-names)
            =>
            (lambda (n)
              (newref
                (proc-val
                  (procedure
                    (list-ref b-varss n)
                    (list-ref p-bodies n)
                    env
                  )
                )
              )
            )
          )
          (else (apply-env saved-env search-name))
        )
      )
      (extend-env-with-self-and-super (self super-name saved-env)
        (case search-name
          ((%self) self)
          ((%super) super-name)
          (else (apply-env saved-env search-name))
        )
      )
    )
  )
)

