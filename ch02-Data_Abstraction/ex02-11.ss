(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define empty-env
  (lambda ()
    '()
  )
)
(define empty-env?
  (lambda (env)
    (null? env)
  )
)
(define extend-env
  (lambda (var val env)
    (extend-env* (list var) (list val) env)
  )
)
(define extend-env*
  (lambda (vars vals env)
    (cons (cons vars vals) env)
  )
)
(define apply-env
  (lambda (env search-var)
    (if (empty-env? env)
      (report-no-bounding-found search-var)
      (let ([rib (car env)])
        (let f ([vars (car rib)] [vals (cdr rib)])
          (if (null? vars)
            (apply-env (cdr env) search-var)
            (if (eqv? (car vars) search-var)
              (car vals)
              (f (cdr vars) (cdr vals))
            )
          )
        )
      )
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

(define env1
  (extend-env* '(a b c) '(11 12 13)
    (extend-env* '(x z) '(66 77)
      (extend-env* '(x y) '(88 99)
        (empty-env)
      )
    )
  )
)
; (print env1)
; (print
;   (extend-env 'd 6
;     (extend-env 'y 8
;       (extend-env 'x 7
;         (extend-env 'y 14 (empty-env))
;       )
;     )
;   )
; )
(print
  (apply-env env1 'x)
) ; 66
(print
  (apply-env env1 'y)
) ; 99
