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
    )
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
      (let-exp (var exp1 vars exps body)
        (let ([vars (cons var vars)] [exps (cons exp1 exps)])
          (value-of body
            (extend-env* vars
              (map (lambda (x) (value-of x env)) exps)
              env
            )
          )
        )
      )
      (letrec-exp (
          p-result-type p-name b-vars b-var-types p-body
          p-result-types p-names b-varss b-var-typess p-bodys letrec-body
        )
        (let (
            [p-names (cons p-name p-names)]
            [b-varss (cons b-vars b-varss)]
            [p-bodys (cons p-body p-bodys)]
          )
          (value-of
            letrec-body
            (extend-env-rec* p-names b-varss p-bodys env)
          )
        )
      )
      (proc-exp (vars var-types body)
        (proc-val (procedure vars body env))
      )
      (call-exp (rator rands)
        (let
          (
            [proc1 (expval->proc (value-of rator env))]
            [args (map (lambda (x) (value-of x env)) rands)]
          )
          (apply-procedure proc1 args)
        )
      )
    )
  )
)

; Proc * Listof(ExpVal) -> ExpVal
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of body (extend-env* vars vals saved-env))
      )
    )
  )
)
