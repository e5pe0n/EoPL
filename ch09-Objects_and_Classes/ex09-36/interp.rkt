#lang eopl

(require "lang.rkt")
(require "data-structs.rkt")
(require "env.rkt")
(require "store.rkt")
(require "classes.rkt")

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
