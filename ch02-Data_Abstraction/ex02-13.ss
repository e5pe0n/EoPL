(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define empty-env
  (lambda ()
    (list
      (lambda (search-var)
        (error 'empty-env "not found: -s" search-var)
      )
      (lambda ()
        #t
      )
    )
  )
)
(define empty-env?
  (lambda (env)
    ((cadr env))
  )
)
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list
      (lambda (search-var)
        (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)
        )
      )
      (lambda ()
        #f
      )
    )
  )
)
(define apply-env
  (lambda (env search-var)
    ((car env) search-var)
  )
)
(define has-binding?
  (lambda (env var)
    (if (empty-env? env)
      #f
      ((car env) search-var)
    )
  )
)

(define e
  (extend-env 'd 6
    (extend-env 'y 8
      (extend-env 'x 7
        (extend-env 'y 14 (empty-env))
      )
    )
  )
)
(print
  (apply-env e 'x)
) ; 7
(print
  (apply-env e 'y)
) ; 8
(print
  (empty-env? e)
) ; #t
(print
  (empty-env? (empty-env))
) ; #f