(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define empty-env
  (lambda ()
    (list 'empty-env')
  )
)
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)
  )
)
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env) (report-no-bouning-found search-var))
      (
        (eqv? (car env) 'extend-env)
        (let (
            [saved-var (cadr env)]
            [saved-val (caddr env)]
            [saved-env (cadddr env)]
          )
          (if (eqv? search-var saved-var)
            saved-val
            (apply-env saved-env search-var)
          )
        )
      )
      (else (report-invalid-env env))
    )
  )
)
(define report-no-binding-found
  (lambda (search-var)
    (error (list 'apply-env ": No binding for" search-var))
  )
)
(define report-invalid-env
  (lambda (env)
    (error (list 'apply-env ": Bad environment=" env))
  )
)