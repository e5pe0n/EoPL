(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define-datatype env env?
  (empty-env)
  (extend-env
    (saved-var symbol?)
    (saved-val s-val?)
    (saved-env env?)
  )
)
(define s-val?
  (lambda (val)
    #t
  )
)
(define apply-env
  (lambda (env search-var)
    (cases env e
      (empty-env () (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)
        )
      )
    )
  )
)
(define has-binding?
  (lambda (env var)
    (cases env e
      (empty-env () #f)
      (extend-env (saved-var saved-val saved-env)
        (or (eqv? var saved-var)
          (has-binding? saved-env var)
        )
      )
    )
  )
)
(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env ": No binding for -s" search-var)
  )
)
(define report-invalid-env
  (lambda (env)
    (error 'apply-env ": Bad environment=-s" env)
  )
)