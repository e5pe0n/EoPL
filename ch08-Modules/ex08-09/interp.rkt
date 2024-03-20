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
      (a-program (m-defns m-depss body)
        (let ([m-deps (extract-m-deps m-depss)])
          (value-of body
            (add-module-defns-to-env m-defns (empty-env))
            m-deps
          )
        )
      )
    )
  )
)

; Listof(ModuleDefn) * Env -> Env
(define add-module-defns-to-env
  (lambda (module-defns env)
    (if (null? module-defns)
      env
      (cases module-definition (car module-defns)
        (a-module-definition (m-name iface m-body)
          (add-module-defns-to-env
            (cdr module-defns)
            (extend-env
              m-name
              (typed-module-val (value-of-module-body iface m-body env))
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
          (defns-module-body (depss defns)
            (simple-module (defns-to-env decls defns env (extract-m-deps depss)))
          )
          (let-defns-module-body (vars exps defns)
            (simple-module
              (defns-to-env decls defns
                (extend-env* vars
                  (map
                    (lambda (exp1) (value-of exp1 env '()))
                    exps
                  )
                  env
                )
                '()
              )
            )
          )
          (letrec-defns-module-body (p-result-types p-names b-varss b-var-typess p-bodies defns)
            (simple-module
              (defns-to-env decls defns
                (extend-env-rec p-names b-varss p-bodies env)
                '()
              )
            )
          )
          (a-module-body-with-local-modules (module-defn module-defns defns)
            (simple-module
              (defns-to-env decls defns
                (add-module-defns-to-env (cons module-defn module-defns) env)
                '()
              )
            )
          )
        )
      )
    )
  )
)

; Listof(Decl) * Listof(Defn) * Env * Listof(ModuleDep) -> Env
(define defns-to-env
  (lambda (decls defns env m-deps)
    (if (null? defns)
      (empty-env)
      (cases definition (car defns)
        (val-defn (var exp)
          (let ([val (value-of exp env m-deps)])
            (let ([new-env (extend-env var val env)])
              (if (variable-name->maybe-binding-in-decls decls var)
                (extend-env var val
                  (defns-to-env decls (cdr defns) new-env m-deps)
                )
                (defns-to-env decls (cdr defns) new-env m-deps)
              )
            )
          )
        )
      )
    )
  )
)

; Exp x Env x Listof(ModuleDep) -> ExpVal
(define value-of
  (lambda (exp env m-deps)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var m-deps))
      (qualified-var-exp (m-name var vars)
        (let loop ([vars (cons m-name (cons var vars))] [env env])
          (let ([val (apply-env env (car vars) m-deps)])
            (if (= (length vars) 1)
              val
              (cases typed-module (expval->typed-module val)
                (simple-module (bindings)
                  (loop (cdr vars) bindings)
                )
              )
            )
          )
        )
      )
      (diff-exp (exp1 exp2)
        (num-val
          (-
            (expval->num (value-of exp1 env m-deps))
            (expval->num (value-of exp2 env m-deps))
          )
        )
      )
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 env m-deps))))
      )
      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env m-deps))
          (value-of exp2 env m-deps)
          (value-of exp3 env m-deps)
        )
      )
      (let-exp (vars exps body)
        (value-of body
          (extend-env* vars (map (lambda (exp1) (value-of exp1 env)) exps) env)
          m-deps
        )
      )
      (letrec-exp (p-result-types p-names b-varss b-var-typess p-bodies letrec-body)
        (value-of
          letrec-body
          (extend-env-rec p-names b-varss p-bodies env)
          m-deps
        )
      )
      (proc-exp (vars var-types body)
        (proc-val (procedure vars body env))
      )
      (call-exp (rator rands)
        (let
          (
            [proc1 (expval->proc (value-of rator env m-deps))]
            [args (map (lambda (rand) (value-of rand env m-deps)) rands)]
          )
          (apply-procedure proc1 args m-deps)
        )
      )
    )
  )
)

; Proc * Listof(ExpVal) -> ExpVal
(define apply-procedure
  (lambda (proc1 vals m-deps)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of body (extend-env* vars vals saved-env) m-deps)
      )
    )
  )
)
