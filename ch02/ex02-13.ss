(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define empty-env
  (lambda ()
    (lambda (search-var)
      (error 'empty-env "not found: -s" search-var)
    )
  )
)

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
        saved-val
        (apply-env search-var saved-env)
      )
    )
  )
)

(define apply-env
  (lambda (env search-var)
    (env search-var)
  )
)