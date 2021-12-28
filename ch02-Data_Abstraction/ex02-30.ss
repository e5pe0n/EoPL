(import datatype)

(define identifier?
  (lambda (x)
    (symbol? x)
  )
)
(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?)
  )
  (lambda-exp
    (bound-var identifier?)
    (body lc-exp?)
  )
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)
  )
)
(define lambda-exp?
  (lambda (datum)
    (and (pair? datum)
      (eqv? (car datum) 'lambda)
      (let ([bv (car datum)])
        (and (pair? bv)
          (identifier? (car bv))
          (null? (cdr bv))
        )
      )
      (lc-exp? (caddr datum))
    )
  )
)
(lambda app-exp?
  (lambda (datum)
    (and (pair? datum)
      (lc-exp? (car datum))
      (lc-exp? (cdr datum))
    )
  )
)
(define report-invalid-concrete-syntax
  (lambda (x)
    (error 'parse-expression "invalid syntax: -s" x)
  )
)
(define parse-expression
  (lambda (datum)
    (cond
      ((identifier? datum) (var-exp datum))
      ((lambda-exp? datum)
        (lambda-exp
          (parse-expression (car (cadr datum)))
          (parse-expresion (caddr datum))
        )
      )
      ((app-exp? datum)
        (app-exp
          (parse-expression (car datum))
          (parse-expression (cadr datum))
        )
      )
      (else (report-invalid-concrete-syntax datum))
    )
  )
)