#lang eopl

(require "lang.rkt")
(require "data-structs.rkt")
(require "env.rkt")
(require "store.rkt")
(require "classes.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (class-decls body)
        (initialize-class-env! class-decls)
        (value-of body (init-env))
      )
    )
  )
)

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (deref (apply-env env var)))
      (diff-exp (exp1 exp2)
        (let (
            [val1 (expval->num (value-of exp1 env))]
            [val2 (expval->num (value-of exp2 env))]
          )
          (num-val (- val1 val2))
        )
      )
      (sum-exp (exp1 exp2)
        (let (
            [val1 (expval->num (value-of exp1 env))]
            [val2 (expval->num (value-of exp2 env))]
          )
          (num-val (+ val1 val2))
        )
      )
      (zero?-exp (exp1)
        (let ([val1 (expval->num (value-of exp1 env))])
          (bool-val (zero? val1))
        )
      )
      (if-exp (exp0 exp1 exp2)
        (if (expval->bool (value-of exp0 env))
          (value-of exp1 env)
          (value-of exp2 env)
        )
      )
      (let-exp (vars exps body)
        (let (
            [
              new-env
              (extend-env vars
                (map newref (values-of-exps exps env))
                env
              )
            ]
          )
          (value-of body new-env)
        )
      )
      (proc-exp (b-vars body)
        (proc-val
          (procedure b-vars body env)
        )
      )
      (call-exp (rator rands)
        (let (
            [proc (expval->proc (value-of rator env))]
            [args (values-of-exps rands env)]
          )
          (apply-procedure proc args)
        )
      )
      (letrec-exp (p-names b-varss p-bodies letrec-body)
        (value-of letrec-body
          (extend-env-rec** p-names b-varss p-bodies env)
        )
      )
      (begin-exp (exp1 exps)
        (letrec (
            [
              value-of-begins
              (lambda (e1 es)
                (let ([v1 (value-of e1 env)])
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
            (value-of exp1 env)
          )
          (num-val 27)
        )
      )
      (list-exp (exps)
        (list-val (values-of-exps exps env))
      )
      (cons-exp (exp1 exp2)
        (let ([v1 (value-of exp1 env)] [v2 (value-of exp2 env)])
          (list-val (cons v1 (expval->list v2)))
        )
      )
      (car-exp (exp1)
        (let ([v1 (value-of exp1 env)])
          (car (expval->list v1))
        )
      )
      (cdr-exp (exp1)
        (let ([v1 (value-of exp1 env)])
          (list-val (cdr (expval->list v1)))
        )
      )
      (null?-exp (exp1)
        (let ([v1 (value-of exp1 env)])
          (bool-val (null? (expval->list v1)))
        )
      )
      (emptylist-exp ()
        (list-val '())
      )
      (print-exp (exp1)
        (let ([v1 (value-of exp1 env)])
          (begin
            (println v1)
            29
          )
        )
      )
      (new-object-exp (class-name rands)
        (let (
            [args (values-of-exps rands env)]
            [obj (new-object class-name)]
          )
          (apply-method
            (find-method class-name 'initialize)
            obj
            args
          )
          obj
        )
      )
      (self-exp ()
        (apply-env env '%self)
      )
      (method-call-exp (obj-exp method-name rands)
        (let (
            [args (values-of-exps rands env)]
            [obj (value-of obj-exp env)]
          )
          (let* ([class-name (object->class-name obj)] [m (find-method class-name method-name)])
            (cases method m
              (a-method (final modifier vars body super-name field-names)
                (if (access-allowed class-name modifier env)
                  (apply-method m obj args)
                  (report-not-allowed-method-call 'method-call class-name method-name)
                )
              )
            )
          )
        )
      )
      (named-method-call-exp (class-name obj-exp method-name rands)
        (let (
            [args (values-of-exps rands env)]
            [obj (value-of obj-exp env)]
          )
          (apply-method
            (find-method class-name method-name)
            obj
            args
          )
        )
      )
      (super-call-exp (method-name rands)
        (let (
            [args (values-of-exps rands env)]
            [obj (apply-env env '%self)]
          )
          (apply-method
            (find-method (apply-env env '%super) method-name)
            obj
            args
          )
        )
      )
      (instanceof-exp (exp1 c-name)
        (if (eqv? c-name 'object)
          #t
          (let ([obj (value-of exp1 env)])
            (if (eqv? (object->class-name obj) c-name)
              #t
              (let f ([super-name (class->super-name (lookup-class (object->class-name obj)))])
                (if (eqv? super-name c-name)
                  #t
                  (and
                    (not (eqv? super-name 'object))
                    (f (class->super-name (lookup-class super-name)))
                  )
                )
              )
            )
          )
        )
      )
      (fieldref-exp (obj-exp field-name)
        (let ([obj (value-of obj-exp env)])
          (let ([pos (fieldpos obj field-name)] [class-name (object->class-name obj)])
            (if pos
              (let ([modifier (list-ref (class->field-modifiers (lookup-class class-name)) pos)])
                (if (access-allowed class-name modifier env)
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
        (let ([obj (value-of obj-exp env)] [v1 (value-of exp1 env)])
          (let ([pos (fieldpos obj field-name)] [class-name (object->class-name obj)])
            (if pos
              (let ([modifier (list-ref (class->field-modifiers (lookup-class class-name)) pos)])
                (if (access-allowed class-name modifier env)
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
              [obj (value-of obj-exp env)]
              [self-name (object->class-name obj)]
              [self (lookup-class self-name)]
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
                (f super-name (class->super-name (lookup-class super-name))
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
              [obj (value-of obj-exp env)]
              [self-name (object->class-name obj)]
              [self (lookup-class self-name)]
              [field-names (class->field-names self)]
            )
            (let f ([self-name self-name] [super-name (class->super-name self)] [prefix #f])
              (if (eqv? self-name class-name)
                (let* (
                    [field-name (if prefix (mangle-field-name prefix field-name) field-name)]
                    [pos (position (lambda (x) (eqv? x field-name)) field-names)]
                  )
                  (if pos
                    (setref! (list-ref (object->fields obj) pos) (value-of exp1 env))
                    (eopl:error 'named-fieldref-exp "no such field ~s in ~s" field-name (object->class-name obj))
                  )
                )
                (f super-name (class->super-name (lookup-class super-name))
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
  (lambda (proc1 args)
    (cases proc proc1
      (procedure (vars body saved-env)
        (let ([new-env (extend-env vars (map newref args) saved-env)])
          (value-of body new-env)
        )
      )
    )
  )
)

(define apply-method
  (lambda (m self args)
    (cases method m
      (a-method (final modifier vars body super-name field-names)
        (value-of body
          (extend-env vars (map newref args)
            (extend-env-with-self-and-super self super-name
              (extend-env field-names (object->fields self)
                (empty-env)
              )
            )
          )
        )
      )
    )
  )
)

(define values-of-exps
  (lambda (exps env)
    (map (lambda (exp) (value-of exp env)) exps)
  )
)

(define store->readable
  (lambda (xs)
    (map (lambda (x) (cons (car x) (expval->printable (cadr x)))) xs)
  )
)

(define access-allowed
  (lambda (class-name modifier env)
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
              (let f ([c-name (class->super-name (lookup-class (object->class-name maybe-self)))])
                (if (eqv? c-name 'object)
                  #f
                  (if (eqv? c-name class-name)
                    #t
                    (f (class->super-name (lookup-class c-name)))
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
