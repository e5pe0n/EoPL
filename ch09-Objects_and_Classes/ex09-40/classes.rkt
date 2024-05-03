#lang eopl

(require "store.rkt")
(require "lang.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(define-datatype object object?
  (an-object
    (class-name identifier?)
    (fields (list-of reference?))
  )
)
(define object->class-name
  (lambda (obj)
    (cases object obj
      (an-object (class-name fields) class-name)
    )
  )
)
(define object->fields
  (lambda (obj)
    (cases object obj
      (an-object (class-name fields) fields)
    )
  )
)

(define-datatype method method?
  (a-method
    (vars (list-of symbol?))
    (body expression?)
    (super-name symbol?)
    (field-names (list-of symbol?))
  )
)
(define method-environment?
  (list-of
    (lambda (p)
      (and
        (pair? p)
        (symbol? (car p))
        (method? (cadr p))
      )
    )
  )
)
(define merge-method-envs
  (lambda (super-m-env new-m-env)
    (append new-m-env super-m-env)
  )
)
(define method-decls->method-env
  (lambda (m-decls super-name field-names)
    (map
      (lambda (m-decl)
        (cases method-decl m-decl
          (a-method-decl (result-type method-name vars var-types body)
            (list method-name
              (a-method vars body super-name field-names)
            )
          )
        )
      )
      m-decls
    )
  )
)
