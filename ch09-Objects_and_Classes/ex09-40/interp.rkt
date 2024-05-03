#lang eopl

(require "lang.rkt")
(require "data-structs.rkt")
(require "env.rkt")
(require "store.rkt")
(require "classes.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(define new-object
  (lambda (class-name)
    (an-object
      class-name
      (map newref (class->field-init-vals (lookup-class class-name)))
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

(define-datatype class class?
  (a-class
    (super-name (maybe symbol?))
    (field-names (list-of symbol?))
    (field-init-vals (list-of expval?))
    (method-env method-environment?)
  )
)
(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-names field-init-vals method-env)
        super-name
      )
    )
  )
)
(define class->field-names
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-names field-init-vals method-env)
        field-names
      )
    )
  )
)
(define class->field-init-vals
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-names field-init-vals method-env)
        field-init-vals
      )
    )
  )
)
(define class->method-env
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-names field-init-vals method-env)
        method-env
      )
    )
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
      (an-interface-decl (i-name parent-i-names method-decls) '())
      (a-class-decl (c-name s-name i-names f-types f-names f-exps m-decls)
        (let (
            [f-names
              (append-field-names (class->field-names (lookup-class s-name)) f-names)
            ]
            [f-init-vals
              (append (values-of-exps f-exps (init-env)) (class->field-init-vals (lookup-class s-name)))
            ]
          )
          (add-to-class-env! c-name
            (a-class s-name f-names f-init-vals
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
      (proc-exp (b-vars b-var-types body)
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
      (letrec-exp (result-type p-names b-varss b-var-typess p-bodies letrec-body)
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
      (list-exp (exp1 exps)
        (list-val
          (cons
            (value-of exp1 env)
            (values-of-exps exps env)
          )
        )
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
          (apply-method
            (find-method (object->class-name obj) method-name)
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
      (cast-exp (exp1 c-name)
        (let ([obj (value-of exp1 env)])
          (if (is-subclass? (object->class-name obj) c-name)
            obj
            (report-cast-error c-name obj)
          )
        )
      )
      (instanceof-exp (exp1 c-name)
        (let ([obj (value-of exp1 env)])
          (bool-val (is-subclass? (object->class-name obj) c-name))
        )
      )
    )
  )
)

(define report-cast-error
  (lambda (c-name obj)
    (eopl:error 'value-of "can't cast object to type ~s:~%~s" c-name obj)
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
      (a-method (vars body super-name field-names)
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

(define is-subclass?
  (lambda (c-name1 c-name2)
    (if (eqv? c-name1 c-name2)
      #t
      (let ([s-name (class->super-name (lookup-class c-name1))])
        (if (eqv? s-name 'object)
          #f
          (is-subclass? s-name c-name2)
        )
      )
    )
  )
)
