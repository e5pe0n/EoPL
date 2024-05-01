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

(define fieldpos
  (lambda (obj field-name)
    (let ([cls (lookup-class (object->class-name obj))])
      (position (lambda (x) (eqv? x field-name)) (class->field-names cls))
    )
  )
)

(define modifier?
  (lambda (v)
    (memq v (list 'private 'protected 'public))
  )
)

(define-datatype method method?
  (a-method
    (modifier modifier?)
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
          (a-method-decl (modifier method-name vars body)
            (list method-name
              (a-method modifier vars body super-name field-names)
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
    (field-modifiers (list-of modifier?))
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
        (list 'object (a-class #f '() '() '()))
      )
    )
    (for-each initialize-class-decl! c-decls)
  )
)
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (c-name s-name f-modifiers f-names m-decls)
        (let (
            [f-modifiers (append (class->field-modifiers (lookup-class s-name)) f-modifiers)]
            [f-names
              (append-field-names s-name (class->field-names (lookup-class s-name)) f-names)
            ]
          )
          (add-to-class-env! c-name
            (a-class s-name f-modifiers f-names
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
  (lambda (super-name super-fields new-fields)
    (cond
      ((null? super-fields) new-fields)
      (else
        (cons
          (if (memq (car super-fields) new-fields)
            (mangle-field-name super-name (car super-fields))
            (car super-fields)
          )
          (append-field-names super-name (cdr super-fields) new-fields)
        )
      )
    )
  )
)
(define mangle-field-name
  (lambda (super-name field-name)
    (string->symbol
      (string-append
        (symbol->string field-name)
        "%"
        (symbol->string super-name)
      )
    )
  )
)

(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-modifiers field-names method-env)
        super-name
      )
    )
  )
)
(define class->field-modifiers
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-modifiers field-names method-env)
        field-modifiers
      )
    )
  )
)
(define class->field-names
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-modifiers field-names method-env)
        field-names
      )
    )
  )
)
(define class->method-env
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-modifiers field-names method-env)
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

(define maybe
  (lambda (pred)
    (lambda (v)
      (or (not v) (pred v))
    )
  )
)
