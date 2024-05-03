#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "static-data-structs.rkt")

(provide (all-defined-out))


(define method-tenv?
  (list-of
    (lambda (p)
      (and
        (pair? p)
        (symbol? (car p))
        (type? (cadr p))
      )
    )
  )
)

(define-datatype static-class static-class?
  (a-static-class
    (super-name (maybe identifier?))
    (interface-names (list-of identifier?))
    (field-names (list-of identifier?))
    (field-types (list-of type?))
    (method-tenv method-tenv?)
  )
  (an-interface
    (method-tenv method-tenv?)
  )
)

(define maybe-find-method-type
  (lambda (m-tenv m-name)
    (cond
      ((assq m-name m-tenv) => cadr)
      (else #f)
    )
  )
)
(define find-method-type
  (lambda (class-name m-name)
    (let (
        [
          m
          (maybe-find-method-type
            (static-class->method-tenv (lookup-static-class class-name))
            m-name
          )
        ]
      )
      (if m
        m
        (eopl:error 'find-method-type "unknown method ~s in class ~s" m-name class-name)
      )
    )
  )
)

(define the-static-class-env '())

(define is-static-class?
  (lambda (name)
    (assq name the-static-class-env)
  )
)
(define is-static-concrete-class?
  (lambda (name)
    (let ([maybe-sc (assq name the-static-class-env)])
      (if maybe-sc
        (cases static-class (cadr maybe-sc)
          (a-static-class (s-name i-names f-names f-types method-tenv) #t)
          (else #f)
        )
        #f
      )
    )
  )
)

(define lookup-static-class
  (lambda (name)
    (cond
      ((assq name the-static-class-env)
        => (lambda (pair) (cadr pair))
      )
      (else (eopl:error 'lookup-static-class "unknown class: ~s" name))
    )
  )
)
(define empty-the-static-class-env!
  (lambda ()
    (set! the-static-class-env '())
  )
)
(define add-static-class-binding!
  (lambda (name sc)
    (set! the-static-class-env
      (cons
        (list name sc)
        the-static-class-env
      )
    )
  )
)

(define initialize-static-class-env!
  (lambda (c-decls)
    (empty-the-static-class-env!)
    (add-static-class-binding!
      'object (a-static-class #f '() '() '() '())
    )
    (for-each add-class-decl-to-static-class-env! c-decls)
  )
)
(define add-class-decl-to-static-class-env!
  (lambda (c-decl)
    (cases class-decl c-decl
      (an-interface-decl (i-name abs-m-decls)
        (let ([m-tenv (abs-method-decls->method-tenv abs-m-decls)])
          (check-no-dups! (map car m-tenv) i-name)
          (add-static-class-binding! i-name (an-interface m-tenv))
        )
      )
      (a-class-decl (c-name s-name i-names f-types f-names m-decls)
        (let (
            [
              i-names
              (append
                (static-class->interface-names (lookup-static-class s-name))
                i-names
              )
            ]
            [
              f-names
              (append-field-names
                (static-class->field-names (lookup-static-class s-name))
                f-names
              )
            ]
            [
              f-types
              (append
                (static-class->field-types (lookup-static-class s-name))
                f-types
              )
            ]
            [
              method-tenv
              (let ([local-method-tenv (method-decls->method-tenv m-decls)])
                (check-no-dups! (map car local-method-tenv) c-name)
                (merge-method-tenvs
                  (static-class->method-tenv (lookup-static-class s-name))
                  local-method-tenv
                )
              )
            ]
          )
          (check-no-dups! i-names c-name)
          (check-no-dups! f-names c-name)
          (check-for-initialize! method-tenv c-name)
          (add-static-class-binding! c-name
            (a-static-class s-name i-names f-names f-types method-tenv)
          )
        )
      )
    )
  )
)

(define abs-method-decls->method-tenv
  (lambda (abs-m-decls)
    (map
      (lambda (abs-m-decl)
        (cases abstract-method-decl abs-m-decl
          (an-abstract-method-decl (result-type m-name arg-ids arg-types)
            (list m-name (proc-type arg-types result-type))
          )
        )
      )
      abs-m-decls
    )
  )
)
(define method-decls->method-tenv
  (lambda (m-decls)
    (map
      (lambda (m-decl)
        (cases method-decl m-decl
          (a-method-decl (result-type m-name arg-ids arg-types body)
            (list m-name (proc-type arg-types result-type))
          )
        )
      )
      m-decls
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
(define merge-method-tenvs
  (lambda (super-tenv new-tenv)
    (append new-tenv super-tenv)
  )
)

(define check-for-initialize!
  (lambda (method-tenv class-name)
    (when (not (maybe-find-method-type method-tenv 'initialize))
      (eopl:error 'check-for-initialize! "no initialize method in class ~s" class-name)
    )
  )
)


(define static-class->super-name
  (lambda (sc)
    (cases static-class sc
      (a-static-class (super-name i-names f-names f-types method-tenv)
        super-name
      )
      (else (report-static-class-extractor-error 'super-name sc))
    )
  )
)
(define static-class->interface-names
  (lambda (sc)
    (cases static-class sc
      (a-static-class (super-name i-names f-names f-types method-tenv)
        i-names
      )
      (else (report-static-class-extractor-error 'interface-names sc))
    )
  )
)
(define static-class->field-names
  (lambda (sc)
    (cases static-class sc
      (a-static-class (super-name i-names f-names f-types method-tenv)
        f-names
      )
      (else (report-static-class-extractor-error 'field-names sc))
    )
  )
)
(define static-class->field-types
  (lambda (sc)
    (cases static-class sc
      (a-static-class (super-name i-names f-names f-types method-tenv)
        f-types
      )
      (else (report-static-class-extractor-error 'field-types sc))
    )
  )
)
(define static-class->method-tenv
  (lambda (sc)
    (cases static-class sc
      (a-static-class (super-name i-names f-names f-types method-tenv)
        method-tenv
      )
      (an-interface (method-tenv) method-tenv)
    )
  )
)
(define report-static-class-extractor-error
  (lambda (kind sc)
    (eopl:error 'static-class-extractors "can't take ~s of interface ~s" kind sc)
  )
)

(define check-no-dups!
  (lambda (xs blamee)
    (cond
      ((null? xs) #t)
      ((memv (car xs) (cdr xs))
        (eopl:error 'check-no-dups! "duplicate found among ~s in class ~s" xs blamee)
      )
      (else (check-no-dups! (cdr xs) blamee))
    )
  )
)

