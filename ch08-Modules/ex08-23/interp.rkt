#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "data-structures.rkt")
(require "env.rkt")

(provide (all-defined-out))

; Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (m-defns body)
        (value-of body
          (add-module-defns-to-env m-defns (empty-env))
        )
      )
    )
  )
)

; Listof(Defn) * Env -> Env
(define add-module-defns-to-env
  (lambda (defns env)
    (if (null? defns)
      env
      (cases module-definition (car defns)
        (a-module-definition (m-name iface m-body)
          (add-module-defns-to-env
            (cdr defns)
            (extend-env-with-module
              m-name
              (value-of-module-body m-body env)
              env
            )
          )
        )
      )
    )
  )
)

; ModuleBody * Env -> TypedModule
(define value-of-module-body
  (lambda (m-body env)
    (cases module-body m-body
      (defns-module-body (defns)
        (simple-module (defns-to-env defns env))
      )
      (var-module-body (m-name)
        (lookup-module-name-in-env m-name env)
      )
      (proc-module-body (m-name m-type m-body)
        (proc-module m-name m-body env)
      )
      (app-module-body (rator rand)
        (let (
            [rator-val (lookup-module-name-in-env rator env)]
            [rand-val (lookup-module-name-in-env rand env)]
          )
          (cases typed-module rator-val
            (proc-module (m-name m-body env)
              (value-of-module-body m-body
                (extend-env-with-module m-name rand-val env)
              )
            )
            (else (report-bad-module-app rator-val))
          )
        )
      )
    )
  )
)

(define report-bad-module-app
  (lambda (v)
    (eopl:error 'bad-module-app "proc-module expected. ~s given" v)
  )
)

; Listof(Defn) * Env -> Env
(define defns-to-env
  (lambda (defns env)
    (if (null? defns)
      (empty-env)
      (cases definition (car defns)
        (val-defn (var exp)
          (let ([val (value-of exp env)])
            (let ([new-env (extend-env var val env)])
              (extend-env var val
                (defns-to-env (cdr defns) new-env)
              )
            )
          )
        )
        (type-defn (type-name type)
          (defns-to-env (cdr defns) env)
        )
      )
    )
  )
)

; Exp x Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (qualified-var-exp (m-name var)
        (lookup-qualified-var-in-env m-name var env)
      )
      (diff-exp (exp1 exp2)
        (num-val
          (-
            (expval->num (value-of exp1 env))
            (expval->num (value-of exp2 env))
          )
        )
      )
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 env))))
      )
      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env))
          (value-of exp2 env)
          (value-of exp3 env)
        )
      )
      (let-exp (var exp1 body)
        (value-of body
          (extend-env var (value-of exp1 env) env)
        )
      )
      (letrec-exp (p-result-type p-name b-var b-var-type p-body letrec-body)
        (value-of
          letrec-body
          (extend-env-rec p-name b-var p-body env)
        )
      )
      (proc-exp (var var-type body)
        (proc-val (procedure var body env))
      )
      (call-exp (rator rand)
        (let
          (
            [proc1 (expval->proc (value-of rator env))]
            [arg (value-of rand env)]
          )
          (apply-procedure proc1 arg)
        )
      )
    )
  )
)

; Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body (extend-env var val saved-env))
      )
    )
  )
)
