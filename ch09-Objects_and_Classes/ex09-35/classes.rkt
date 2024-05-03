#lang eopl

(require "store.rkt")
(require "lang.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(define identifier? symbol?)

(define-datatype object object?
  (an-object
    (class-name identifier?)
    (fields (list-of reference?))
  )
)

(define new-object
  (lambda (class-name)
    (an-object
      class-name
      (map
        (lambda (field-name) (newref (list 'uninitialized-field field-name)))
        (class->field-names (lookup-class class-name))
      )
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
(define assq-method-env
  (lambda (m-env id)
    (cond
      ((assq id m-env) => cadr)
    )
  )
)
(define find-method
  (lambda (c-name name)
    (let ([m-env (class->method-env (lookup-class c-name))])
      (let ([maybe-pair (assq name m-env)])
        (if (pair? maybe-pair)
          (cadr maybe-pair)
          (report-method-not-found name)
        )
      )
    )
  )
)
(define report-method-not-found
  (lambda (name)
    (eopl:error 'find-method "unknown method ~s" name)
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

(define-datatype class class?
  (a-class
    (super-name (maybe symbol?))
    (field-names (list-of symbol?))
    (method-env method-environment?)
  )
)
(define the-class-env '())
(define add-to-class-env!
  (lambda (class-name class)
    (set! the-class-env
      (cons
        (list class-name class)
        the-class-env
      )
    )
  )
)
(define lookup-class
  (lambda (name)
    (let ([maybe-pair (assq name the-class-env)])
      (if maybe-pair
        (cadr maybe-pair)
        (report-unknown-class name)
      )
    )
  )
)
(define report-unknown-class
  (lambda (name)
    (eopl:error 'lookup-class "unknown class ~s" name)
  )
)
(define initialize-class-env!
  (lambda (c-decls)
    (set! the-class-env
      (list
        (list 'object (a-class #f '() '()))
      )
    )
    (for-each initialize-class-decl! c-decls)
  )
)
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      (an-interface-decl (i-name method-decls) '())
      (a-class-decl (c-name s-name i-names f-types f-names m-decls)
        (let (
            [f-names
              (append-field-names (class->field-names (lookup-class s-name)) f-names)
            ]
          )
          (add-to-class-env! c-name
            (a-class s-name f-names
              (merge-method-envs
                (class->method-env (lookup-class s-name))
                (method-decls->method-env m-decls s-name f-names)
              )
            )
          )
        )
      )
    )
  )
)
(define append-field-names
  (lambda (super-fields new-fields)
    (cond
      ((null? super-fields) new-fields)
      (else
        (cons
          (if (memq (car super-fields) new-fields)
            (fresh-identifier (car super-fields))
            (car super-fields)
          )
          (append-field-names (cdr super-fields) new-fields)
        )
      )
    )
  )
)

(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-names method-env)
        super-name
      )
    )
  )
)
(define class->field-names
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-names method-env)
        field-names
      )
    )
  )
)
(define class->method-env
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-names method-env)
        method-env
      )
    )
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
