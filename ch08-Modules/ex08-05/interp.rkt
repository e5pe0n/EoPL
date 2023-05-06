#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "static-data-structures.rkt")
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
              (value-of-module-body iface m-body env)
              env
            )
          )
        )
      )
    )
  )
)

; Iface * ModuleBody * Env -> TypedModule
(define value-of-module-body
  (lambda (iface m-body env)
    (cases interface iface
      (simple-iface (decls)
        (cases module-body m-body
          (defns-module-body (defns)
            (simple-module (defns-to-env decls defns env))
          )
          (let-defns-module-body (vars exps defns)
            (simple-module
              (defns-to-env decls defns
                (extend-env* vars
                  (map
                    (lambda (exp1) (value-of exp1 env))
                    exps
                  )
                  env
                )
              )
            )
          )
          (letrec-defns-module-body (p-result-types p-names b-varss b-var-typess p-bodies defns)
            (simple-module
              (defns-to-env decls defns
                (extend-env-rec p-names b-varss p-bodies env)
              )
            )
          )
        )
      )
    )
  )
)

; Listof(Decl) * Listof(Defn) * Env -> Env
(define defns-to-env
  (lambda (decls defns env)
    (if (null? defns)
      (empty-env)
      (cases definition (car defns)
        (val-defn (var exp)
          (let ([val (value-of exp env)])
            (let ([new-env (extend-env var val env)])
              (if (variable-name->maybe-binding-in-decls decls var)
                (extend-env var val
                  (defns-to-env decls (cdr defns) new-env)
                )
                (defns-to-env decls (cdr defns) new-env)
              )
            )
          )
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
      (let-exp (vars exps body)
        (value-of body
          (extend-env* vars (map (lambda (exp1) (value-of exp1 env)) exps) env)
        )
      )
      (letrec-exp (p-result-types p-names b-varss b-var-typess p-bodies letrec-body)
        (value-of
          letrec-body
          (extend-env-rec p-names b-varss p-bodies env)
        )
      )
      (proc-exp (vars var-types body)
        (proc-val (procedure vars body env))
      )
      (call-exp (rator rands)
        (let
          (
            [proc1 (expval->proc (value-of rator env))]
            [args (map (lambda (rand) (value-of rand env)) rands)]
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
