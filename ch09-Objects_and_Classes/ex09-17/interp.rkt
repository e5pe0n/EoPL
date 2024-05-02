#lang eopl

(require "lang.rkt")
(require "data-structs.rkt")
(require "env.rkt")
(require "store.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (class-decls body)
        (initialize-class-env! class-decls)
        (value-of body (init-env) the-class-env)
      )
    )
  )
)

(define value-of
  (lambda (exp env class-env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (deref (apply-env env var)))
      (diff-exp (exp1 exp2)
        (let (
            [val1 (expval->num (value-of exp1 env class-env))]
            [val2 (expval->num (value-of exp2 env class-env))]
          )
          (num-val (- val1 val2))
        )
      )
      (sum-exp (exp1 exp2)
        (let (
            [val1 (expval->num (value-of exp1 env class-env))]
            [val2 (expval->num (value-of exp2 env class-env))]
          )
          (num-val (+ val1 val2))
        )
      )
      (zero?-exp (exp1)
        (let ([val1 (expval->num (value-of exp1 env class-env))])
          (bool-val (zero? val1))
        )
      )
      (if-exp (exp0 exp1 exp2)
        (if (expval->bool (value-of exp0 env class-env))
          (value-of exp1 env class-env)
          (value-of exp2 env class-env)
        )
      )
      (let-exp (vars exps body)
        (let (
            [
              new-env
              (extend-env vars
                (map newref (values-of-exps exps env class-env))
                env
              )
            ]
          )
          (value-of body new-env class-env)
        )
      )
      (letclass-exp (
          class-names super-names
          static-varss static-expss
          field-modifierss field-namess
          method-declss
          body
        )
        (let (
            [
              new-class-env
              (let f (
                  [class-names class-names] [super-names super-names]
                  [static-varss static-varss] [static-expss static-expss]
                  [field-modifierss field-modifierss] [field-namess field-namess]
                  [method-declss method-declss] [class-env class-env]
                )
                (if (null? class-names)
                  class-env
                  (let (
                      [c-name (car class-names)] [s-name (car super-names)]
                      [static-vars (car static-varss)] [static-exps (car static-expss)]
                      [f-modifiers (car field-modifierss)] [f-names (car field-namess)]
                      [m-decls (car method-declss)]
                    )
                    (let (
                        [static-vals (values-of-exps static-exps env class-env)]
                        [f-modifiers (append (class->field-modifiers (lookup-class s-name class-env)) f-modifiers)]
                        [f-names
                          (append-field-names s-name (class->field-names (lookup-class s-name class-env)) f-names)
                        ]
                      )
                      (f (cdr class-names) (cdr super-names)
                        (cdr static-varss) (cdr static-expss)
                        (cdr field-modifierss) (cdr field-namess)
                        (cdr method-declss)
                        (extend-class-env c-name
                          (a-class s-name f-modifiers f-names
                            (merge-method-envs
                              s-name
                              c-name
                              (class->method-env (lookup-class s-name class-env))
                              (method-decls->method-env m-decls c-name s-name f-names)
                            )
                            (extend-env static-vars (map newref static-vals) (empty-env))
                          )
                          class-env
                        )
                      )
                    )
                  )
                )
              )
            ]
          )
          (value-of body env new-class-env)
        )
      )
      (proc-exp (b-vars body)
        (proc-val
          (procedure b-vars body env)
        )
      )
      (call-exp (rator rands)
        (let (
            [proc (expval->proc (value-of rator env class-env))]
            [args (values-of-exps rands env class-env)]
          )
          (apply-procedure proc args class-env)
        )
      )
      (letrec-exp (p-names b-varss p-bodies letrec-body)
        (value-of letrec-body
          (extend-env-rec** p-names b-varss p-bodies env)
          class-env
        )
      )
      (begin-exp (exp1 exps)
        (letrec (
            [
              value-of-begins
              (lambda (e1 es)
                (let ([v1 (value-of e1 env class-env)])
                  (if (null? es)
                    v1
                    (value-of-begins (car es) (cdr es))
                  )
                )
              )
            ]
          )
          (value-of-begins exp1 exps)
        )
      )
      (assign-exp (var exp1)
        (begin
          (setref!
            (apply-env env var)
            (value-of exp1 env class-env)
          )
          (num-val 27)
        )
      )
      (list-exp (exps)
        (list-val (values-of-exps exps env class-env))
      )
      (cons-exp (exp1 exp2)
        (let ([v1 (value-of exp1 env class-env)] [v2 (value-of exp2 env class-env)])
          (list-val (cons v1 (expval->list v2)))
        )
      )
      (car-exp (exp1)
        (let ([v1 (value-of exp1 env class-env)])
          (car (expval->list v1))
        )
      )
      (cdr-exp (exp1)
        (let ([v1 (value-of exp1 env class-env)])
          (list-val (cdr (expval->list v1)))
        )
      )
      (null?-exp (exp1)
        (let ([v1 (value-of exp1 env class-env)])
          (bool-val (null? (expval->list v1)))
        )
      )
      (emptylist-exp ()
        (list-val '())
      )
      (print-exp (exp1)
        (let ([v1 (value-of exp1 env class-env)])
          (begin
            (println v1)
            29
          )
        )
      )
      (new-object-exp (class-name rands)
        (let (
            [args (values-of-exps rands env class-env)]
            [obj (new-object class-name class-env)]
          )
          (apply-method
            (find-method class-name (method-signature 'initialize (length args)) class-env)
            obj
            args
            class-env
          )
          obj
        )
      )
      (self-exp ()
        (apply-env env '%self)
      )
      (method-call-exp (obj-exp method-name rands)
        (let* (
            [args (values-of-exps rands env class-env)]
            [obj (value-of obj-exp env class-env)]
            [
              class-name
              (cases expression obj-exp
                (self-exp () (apply-env env '%host))
                (else (object->class-name obj))
              )
            ]
            [m-sig (method-signature method-name (length args))]
            [m (find-method class-name m-sig class-env)]
          )
          (cases method m
            (a-method (final modifier vars body host-name super-name field-names)
              (if (access-allowed class-name modifier env class-env)
                (apply-method m obj args class-env)
                (report-not-allowed-method-call 'method-call class-name m-sig)
              )
            )
          )
        )
      )
      (named-method-call-exp (class-name obj-exp method-name rands)
        (let (
            [args (values-of-exps rands env class-env)]
            [obj (value-of obj-exp env class-env)]
          )
          (apply-method
            (find-method class-name (method-signature method-name (length args)) class-env)
            obj
            args
            class-env
          )
        )
      )
      (super-call-exp (method-name rands)
        (let (
            [args (values-of-exps rands env class-env)]
            [obj (apply-env env '%self)]
          )
          (apply-method
            (find-method (apply-env env '%super) (method-signature method-name (length args)) class-env)
            obj
            args
            class-env
          )
        )
      )
      (instanceof-exp (exp1 c-name)
        (if (eqv? c-name 'object)
          #t
          (let ([obj (value-of exp1 env class-env)])
            (if (eqv? (object->class-name obj) c-name)
              #t
              (let f ([super-name (class->super-name (lookup-class (object->class-name obj) class-env))])
                (if (eqv? super-name c-name)
                  #t
                  (and
                    (not (eqv? super-name 'object))
                    (f (class->super-name (lookup-class super-name class-env)))
                  )
                )
              )
            )
          )
        )
      )
      (fieldref-exp (obj-exp field-name)
        (let ([obj (value-of obj-exp env class-env)])
          (let ([pos (fieldpos obj field-name class-env)] [class-name (object->class-name obj)])
            (if pos
              (let ([modifier (list-ref (class->field-modifiers (lookup-class class-name class-env)) pos)])
                (if (access-allowed class-name modifier env class-env)
                  (deref (list-ref (object->fields obj) pos))
                  (report-not-allowed-field-access 'fieldref class-name field-name)
                )
              )
              (report-field-not-found 'fieldref class-name field-name)
            )
          )
        )
      )
      (fieldset-exp (obj-exp field-name exp1)
        (let ([obj (value-of obj-exp env class-env)] [v1 (value-of exp1 env class-env)])
          (let ([pos (fieldpos obj field-name class-env)] [class-name (object->class-name obj)])
            (if pos
              (let ([modifier (list-ref (class->field-modifiers (lookup-class class-name class-env)) pos)])
                (if (access-allowed class-name modifier env class-env)
                  (let ([ref (list-ref (object->fields obj) pos)])
                    (setref! ref v1)
                  )
                  (report-not-allowed-field-access 'fieldset class-name field-name)
                )
              )
              (report-field-not-found 'fieldset class-name field-name)
            )
          )
        )
      )
      (named-fieldref-exp (class-name obj-exp field-name)
        (if (eqv? class-name 'object)
          (eopl:error 'named-fieldref-exp "no such field ~s in ~s" field-name class-name)
          (let* (
              [obj (value-of obj-exp env class-env)]
              [self-name (object->class-name obj)]
              [self (lookup-class self-name class-env)]
              [field-names (class->field-names self)]
            )
            (let f ([self-name self-name] [super-name (class->super-name self)] [prefix #f])
              (if (eqv? self-name class-name)
                (let* (
                    [field-name (if prefix (mangle-field-name prefix field-name) field-name)]
                    [pos (position (lambda (x) (eqv? x field-name)) field-names)]
                  )
                  (if pos
                    (deref (list-ref (object->fields obj) pos))
                    (eopl:error 'named-fieldref-exp "no such field ~s in ~s" field-name (object->class-name obj))
                  )
                )
                (f super-name (class->super-name (lookup-class super-name class-env))
                  (if (memq (mangle-field-name super-name field-name) field-names)
                    super-name
                    #f
                  )
                )
              )
            )
          )
        )
      )
      (named-fieldset-exp (class-name obj-exp field-name exp1)
        (if (eqv? class-name 'object)
          (eopl:error 'named-fieldset-exp "no such field ~s in ~s" field-name class-name)
          (let* (
              [obj (value-of obj-exp env class-env)]
              [self-name (object->class-name obj)]
              [self (lookup-class self-name class-env)]
              [field-names (class->field-names self)]
            )
            (let f ([self-name self-name] [super-name (class->super-name self)] [prefix #f])
              (if (eqv? self-name class-name)
                (let* (
                    [field-name (if prefix (mangle-field-name prefix field-name) field-name)]
                    [pos (position (lambda (x) (eqv? x field-name)) field-names)]
                  )
                  (if pos
                    (setref! (list-ref (object->fields obj) pos) (value-of exp1 env class-env))
                    (eopl:error 'named-fieldref-exp "no such field ~s in ~s" field-name (object->class-name obj))
                  )
                )
                (f super-name (class->super-name (lookup-class super-name class-env))
                  (if (memq (mangle-field-name super-name field-name) field-names)
                    super-name
                    #f
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

(define report-not-allowed-method-call
  (lambda (class-name method-name)
    (eopl:error 'method-call-exp "method call not allowed: class ~s, method ~s." class-name method-name)
  )
)

(define apply-procedure
  (lambda (proc1 args class-env)
    (cases proc proc1
      (procedure (vars body saved-env)
        (let ([new-env (extend-env vars (map newref args) saved-env)])
          (value-of body new-env class-env)
        )
      )
    )
  )
)

(define apply-method
  (lambda (m self args class-env)
    (cases method m
      (a-method (final modifier vars body host-name super-name field-names)
        (value-of body
          (extend-env vars (map newref args)
            (extend-env-with-self-and-super-and-host self super-name host-name
              (extend-env field-names (object->fields self)
                (class->static-env (lookup-class (object->class-name self) class-env))
              )
            )
          )
          class-env
        )
      )
    )
  )
)

(define values-of-exps
  (lambda (exps env class-env)
    (map (lambda (exp) (value-of exp env class-env)) exps)
  )
)

(define access-allowed
  (lambda (class-name modifier env class-env)
    (cond
      [
        (eqv? modifier 'private)
        (let ([maybe-self (find-env env '%self)])
          (and maybe-self (eqv? (object->class-name maybe-self) class-name))
        )
      ]
      [
        (eqv? modifier 'protected)
        (let ([maybe-self (find-env env '%self)])
          (and maybe-self
            (or (eqv? (object->class-name maybe-self) class-name)
              (let f ([c-name (class->super-name (lookup-class (object->class-name maybe-self) class-env))])
                (if (eqv? c-name 'object)
                  #f
                  (if (eqv? c-name class-name)
                    #t
                    (f (class->super-name (lookup-class c-name class-env)))
                  )
                )
              )
            )
          )
        )
      ]
      [(eqv? modifier 'public) #t]
    )
  )
)
(define report-not-allowed-field-access
  (lambda (kind class-name field-name)
    (eopl:error kind "field access not allowed: class ~s, field ~s." class-name field-name)
  )
)

(define report-field-not-found
  (lambda (kind class-name field-name)
    (eopl:error kind "no such field ~s in ~s." field-name class-name)
  )
)

(define new-object
  (lambda (class-name class-env)
    (an-object
      class-name
      (map
        (lambda (field-name) (newref (list 'uninitialized-field field-name)))
        (class->field-names (lookup-class class-name class-env))
      )
    )
  )
)
(define fieldpos
  (lambda (obj field-name class-env)
    (let ([cls (lookup-class (object->class-name obj) class-env)])
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
    (final boolean?)
    (modifier modifier?)
    (vars (list-of symbol?))
    (body expression?)
    (host-name symbol?)
    (super-name symbol?)
    (field-names (list-of symbol?))
  )
)
(define method-environment?
  (list-of
    (lambda (p)
      (and
        (pair? p)
        (symbol? (car p)) ; method signature
        (method? (cadr p))
      )
    )
  )
)
(define method-signature
  (lambda (method-name num-args)
    (string->symbol
      (string-append
        (symbol->string method-name)
        "%"
        (number->string num-args)
      )
    )
  )
)
(define find-method
  (lambda (c-name m-sig class-env)
    (let ([m-env (class->method-env (lookup-class c-name class-env))])
      (let ([maybe-pair (assq m-sig m-env)])
        (if (pair? maybe-pair)
          (cadr maybe-pair)
          (report-method-not-found m-sig)
        )
      )
    )
  )
)
(define report-method-not-found
  (lambda (m-sig)
    (eopl:error 'find-method "unknown method ~s" m-sig)
  )
)
(define merge-method-envs
  (lambda (super-name class-name super-m-env new-m-env)
    (check-final-method! super-name class-name super-m-env new-m-env)
    (append new-m-env super-m-env)
  )
)
(define check-final-method!
  (lambda (super-name class-name super-m-env new-m-env)
    (let (
        [
          final-m-env
          (filter
            (lambda (m-env)
              (cases method (cadr m-env)
                (a-method (final modifier vars body host-name super-name field-names) final)
              )
            )
            super-m-env
          )
        ]
      )
      (for-each
        (lambda (m-env)
          (let ([m-sig (car m-env)])
            (if (assq m-sig final-m-env)
              (eopl:error 'check-final-method! "class ~s can't override method ~s in class ~s." class-name m-sig super-name)
              31
            )
          )
        )
        new-m-env
      )
    )
  )
)
(define method-decls->method-env
  (lambda (m-decls class-name super-name field-names)
    (map
      (lambda (m-decl)
        (cases method-decl m-decl
          (a-method-decl (modifier method-name vars body)
            (list (method-signature method-name (length vars))
              (a-method #f modifier vars body class-name super-name field-names)
            )
          )
          (a-final-method-decl (modifier method-name vars body)
            (list (method-signature method-name (length vars))
              (a-method #t modifier vars body class-name super-name field-names)
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
    (static-env environment?)
  )
)
(define the-class-env '())
(define initialize-class-env!
  (lambda (c-decls)
    (set! the-class-env
      (list
        (list 'object (a-class #f '() '() '() (empty-env)))
      )
    )
    (for-each initialize-class-decl! c-decls)
  )
)
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (c-name s-name static-vars static-exps f-modifiers f-names m-decls)
        (let (
            [static-vals (values-of-exps static-exps (empty-env) the-class-env)]
            [f-modifiers (append (class->field-modifiers (lookup-class s-name the-class-env)) f-modifiers)]
            [f-names
              (append-field-names s-name (class->field-names (lookup-class s-name the-class-env)) f-names)
            ]
          )
          (add-to-class-env! c-name
            (a-class s-name f-modifiers f-names
              (merge-method-envs
                s-name
                c-name
                (class->method-env (lookup-class s-name the-class-env))
                (method-decls->method-env m-decls c-name s-name f-names)
              )
              (extend-env static-vars (map newref static-vals) (empty-env))
            )
          )
        )
      )
    )
  )
)
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
(define extend-class-env
  (lambda (class-name class class-env)
    (cons (list class-name class) class-env)
  )
)
(define lookup-class
  (lambda (name class-env)
    (let ([maybe-pair (assq name class-env)])
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
      (a-class (super-name field-modifiers field-names method-env static-env)
        super-name
      )
    )
  )
)
(define class->field-modifiers
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-modifiers field-names method-env static-env)
        field-modifiers
      )
    )
  )
)
(define class->field-names
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-modifiers field-names method-env static-env)
        field-names
      )
    )
  )
)
(define class->method-env
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-modifiers field-names method-env static-env)
        method-env
      )
    )
  )
)
(define class->static-env
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-modifiers field-names method-env static-env)
        static-env
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
