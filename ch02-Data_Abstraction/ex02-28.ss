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

(define parse
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) (symbol->string var))
      (lambda-exp (bound-var body)
        (string-append "lambda (" (symbol->string bound-var) ") " (parse body))
      )
      (app-exp (rator rand)
        (string-append "(" (parse rator) " " (parse rand) ")")
      )
    )
  )
)
(define e1
  (lambda-exp 'x (app-exp (var-exp 'f) (app-exp (var-exp 'f) (var-exp 'x))))
)
(print
  (parse e1)  ; lambda (x) (f (f x))
)