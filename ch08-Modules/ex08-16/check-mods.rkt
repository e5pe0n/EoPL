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

; Sym * Iface * Tenv -> Iface
(define expand-iface
  (lambda (m-name iface tenv)
    (cases interface iface
      (simple-iface (decls)
        (simple-iface (expand-decls m-name decls tenv))
      )
    )
  )
)

; Sym * Listof(Decl) * Tenv -> Listof(Decl)
(define expand-decls
  (lambda (m-name decls internal-tenv)
    (if (null? decls)
      '()
      (cases declaration (car decls)
        (opaque-type-decl (t-name)
          (let ([expanded-type (qualified-type m-name t-name)])
            (let ([new-env (extend-tenv-with-type t-name expanded-type internal-tenv)])
              (cons
                (transparent-type-decl t-name expanded-type)
                (expand-decls m-name (cdr decls) new-env)
              )
            )
          )
        )
        (transparent-type-decl (t-name ty)
          (let ([expanded-type (expand-type ty internal-tenv)])
            (let ([new-env (extend-tenv-with-type t-name expanded-type internal-tenv)])
              (cons
                (transparent-type-decl t-name expanded-type)
                (expand-decls m-name (cdr decls) new-env)
              )
            )
          )
        )
        (val-decl (var-name ty)
          (let ([expanded-type (expand-type ty internal-tenv)])
            (cons
              (val-decl var-name expanded-type)
              (expand-decls m-name (cdr decls) internal-tenv)
            )
          )
        )
      )
    )
  )
)
