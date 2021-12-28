(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      (
        (eqv? (car exp) 'lambda)
        (and
          (not (eqv? var (car (cadr exp))))
          (occurs-free? var (caddr exp))
        )
      )
      (else
        (or
          (occurs-free? var (car exp))
          (occurs-free? var (cadr exp))
        )
      )
    )
  )
)

(print
  (occurs-free? 'x 'x)
)  ; #t
(print
  (occurs-free? 'x 'y)
)  ; #f
(print
  (occurs-free?
    'x
    '(lambda (x) (x y))
  )
) ; #f
(print
  (occurs-free?
    'x
    '(
        (lambda (x) (x y)) (x y)
      )
  )
) ; #t
(print
  (occurs-free?
    'x
    '(lambda (y)
      (lambda (z) (x (y z)))
    )
  )
)  ; #t