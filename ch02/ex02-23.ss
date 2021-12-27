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
    (bound-val identifier?)
    (body lc-exp?)
  )
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)
  )
)