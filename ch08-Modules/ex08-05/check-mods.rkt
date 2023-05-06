#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "static-data-structures.rkt")
(require "checker.rkt")
(require "subtyping.rkt")

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
          (if (module-name->maybe-binding-in-tenv tenv m-name)
            (eopl:error 'module-name-dupulicated "module named ~s is already defined" m-name)
            (let ([actual-iface (interface-of m-body tenv)])
              (if (<:-iface actual-iface expected-iface tenv)
                (let ([new-tenv (extend-tenv-with-module m-name expected-iface tenv)])
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
)

; ModuleBody * Tenv -> Iface
(define interface-of
  (lambda (m-body tenv)
    (cases module-body m-body
      (defns-module-body (defns)
        (simple-iface (defns-to-decls defns tenv))
      )
      (let-defns-module-body (vars exps defns)
        (simple-iface
          (defns-to-decls defns
            (extend-tenv* vars (map (lambda (exp1) (type-of exp1 tenv)) exps) tenv)
          )
        )
      )
      (letrec-defns-module-body (p-result-types p-names b-varss b-var-typess p-bodies defns)
        (let* (
            [tenv
              (extend-tenv* p-names
                (map
                  (lambda (p-result-type b-var-types)
                    (proc-type b-var-types p-result-type)
                  )
                  p-result-types b-var-typess
                )
                tenv
              )
            ]
            [p-body-types
              (map
                (lambda (b-vars b-var-types p-body)
                  (type-of p-body
                    (extend-tenv* b-vars b-var-types tenv)
                  )
                )
                b-varss b-var-typess p-bodies
              )
            ]
          )
          (check-equal-types! p-body-types p-result-types p-bodies)
          (simple-iface (defns-to-decls defns tenv))
        )
      )
    )
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
