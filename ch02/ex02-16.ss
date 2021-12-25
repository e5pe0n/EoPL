; constructors
(define var-exp
  (lambda (var)
    var
  )
)
(define lambda-exp
  (lambda (var exp)
    (list 'lambda var exp)
  )
)
(define app-exp
  (lambda (exp1 exp2)
    (list exp1 exp2)
  )
)

; predicates
(define var-exp?
  (lambda (exp)
    (symbol? exp)
  )
)
(define lambda-exp?
  (lambda (exp)
    (and (list? exp)
      (eqv? (car exp) 'lambda)
      (var-exp? (cadr exp))
      (lc-exp? (caddr exp))
    )
  )
)
(define lc-exp?
  (lambda (exp)
    (or (var-exp? exp) (lambda-exp? exp) (app-exp? exp))
  )
)

; extractors
(define var-exp?
  (lambda (exp)
    exp
  )
)
(define lambda-exp->bound-var
  (lambda (exp)
    (cadr exp)
  )
)
(define lambda-exp->body
  (lambda (exp)
    (caddr exp)
  )
)
(define app->rator
  (lambda (exp)
    (car exp)
  )
)
(define app->rand
  (lambda (exp)
    (cadr exp)
  )
)