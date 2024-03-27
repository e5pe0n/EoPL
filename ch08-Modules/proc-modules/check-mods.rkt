#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "static-data-structures.rkt")
(require "checker.rkt")
(require "subtyping.rkt")
(require "renaming.rkt")

(provide (all-defined-out))

; Program -> Type
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (module-defns body)
        (type-of body
          (add-module-defns-to-tenv module-defns (empty-tenv))
        )
      )
    )
  )
)

; Listof(ModuleDefn) * Tenv -> Tenv
(define add-module-defns-to-tenv
  (lambda (defns tenv)
    (if (null? defns)
      tenv
      (cases module-definition (car defns)
        (a-module-definition (m-name expected-iface m-body)
          (let ([actual-iface (interface-of m-body tenv)])
            (if (<:-iface actual-iface expected-iface tenv)
              (let (
                  [
                    new-tenv
                    (extend-tenv-with-module m-name
                      (expand-iface m-name expected-iface tenv)
                      tenv
                    )
                  ]
                )
                (add-module-defns-to-tenv (cdr defns) new-tenv)
              )
              (report-module-doesnt-satisfy-iface m-name expected-iface actual-iface)
            )
          )
        )
      )
    )
  )
)

; ModuleBody * Tenv -> Iface
(define interface-of
  (lambda (m-body tenv)
    (cases module-body m-body
      (defns-module-body (defns)
        (simple-iface (defns-to-decls defns tenv))
      )
      (var-module-body (m-name)
        (lookup-module-name-in-tenv tenv m-name)
      )
      (proc-module-body (rand-name rand-iface m-body)
        (let (
            [
              body-iface
              (interface-of m-body
                (extend-tenv-with-module rand-name
                  (expand-iface rand-name rand-iface tenv)
                  tenv
                )
              )
            ]
          )
          (proc-iface rand-name rand-iface body-iface)
        )
      )
      (app-module-body (rator-id rand-id)
        (let (
            [rator-iface (lookup-module-name-in-tenv tenv rator-id)]
            [rand-iface (lookup-module-name-in-tenv tenv rand-id)]
          )
          (cases interface rator-iface
            (simple-iface (decls)
              (report-attempt-to-apply-simple-module rator-id)
            )
            (proc-iface (param-name param-iface result-iface)
              (if (<:-iface rand-iface param-iface tenv)
                (rename-in-iface result-iface param-name rand-id)
                (report-bad-module-application-error param-iface rand-iface m-body)
              )
            )
          )
        )
      )
    )
  )
)

(define report-attempt-to-apply-simple-module
  (lambda (v)
    (eopl:error 'attempt-to-apply-simple-module-error "proc-iface expected. ~s given" v)
  )
)

(define report-bad-module-application-error
  (lambda  (param-iface rand-iface m-body)
    (eopl:error 'bad-module-application-error "param-iface <: rand-iface expected. param-iface=~s, rand-iface=~s, m-body=~s" param-iface rand-iface m-body)
  )
)

; Listof(Defn) * Tenv -> Listof(Decl)
(define defns-to-decls
  (lambda (defns tenv)
    (if (null? defns)
      '()
      (cases definition (car defns)
        (val-defn (var-name exp)
          (let ([ty (type-of exp tenv)])
            (let ([new-tenv (extend-tenv var-name ty tenv)])
              (cons
                (val-decl var-name ty)
                (defns-to-decls (cdr defns) new-tenv)
              )
            )
          )
        )
        (type-defn (name ty)
          (let ([new-env (extend-tenv-with-type name (expand-type ty tenv) tenv)])
            (cons
              (transparent-type-decl name ty)
              (defns-to-decls (cdr defns) new-env)
            )
          )
        )
      )
    )
  )
)

; Sym * Iface * IFace -> ()
(define report-module-doesnt-satisfy-iface
  (lambda (m-name expected-iface actual-iface)
    (eopl:pretty-print
      (list 'error-in-defn-of-module: m-name
        'expected-iface: expected-iface
        'actual-iface: actual-iface
      )
    )
    (eopl:error 'type-of-module-defn)
  )
)
