#lang eopl

(require "utils.rkt")
(require "lang.rkt")

(provide (all-defined-out))

(define-datatype t-environment t-environment?
  (empty-tenv)
  (extend-tenv
    (saved-var symbol?)
    (saved-ty type?)
    (saved-tenv t-environment?)
  )
  (extend-tenv-with-module
    (name symbol?)
    (interface interface?)
    (saved-tenv t-environment?)
  )
  (extend-tenv-with-type
    (name symbol?)
    (ty type?)
    (saved-tenv t-environment?)
  )
)

; () -> TEnv
(define init-tenv
  (lambda () (empty-tenv))
)

; Sym * Sym * Tenv -> Type
(define lookup-qualified-var-in-tenv
  (lambda (m-name var-name tenv)
    (let ([iface (lookup-module-name-in-tenv tenv m-name)])
      (cases interface iface
        (simple-iface (decls)
          (lookup-variable-name-in-decls var-name decls)
        )
        (proc-iface (param-name param-iface result-iface)
          (eopl:error 'lookup-qualified-var-in-tenv
            "can't retrieve variable from ~s take ~s from proc-iface" m-name var-name
          )
        )
      )
    )
  )
)

; Tenv * Sym -> Maybe(Type)
(define lookup-variable-name-in-tenv
  (lambda (tenv search-name)
    (let ([maybe-answer (variable-name->maybe-binding-in-tenv tenv search-name)])
      (if maybe-answer
        maybe-answer
        (report-tenv-lookup-failure-error 'variable search-name tenv)
      )
    )
  )
)

; Sym * Tenv -> Maybe(Iface)
(define lookup-module-name-in-tenv
  (lambda (tenv search-name)
    (let ([maybe-answer (module-name->maybe-binding-in-tenv tenv search-name)])
      (if maybe-answer
        maybe-answer
        (report-tenv-lookup-failure-error 'module search-name tenv)
      )
    )
  )
)

(define apply-tenv lookup-variable-name-in-tenv)

; Sym * Sym * Tenv -> ()
(define report-tenv-lookup-failure-error
  (lambda (kind var tenv)
    (eopl:pretty-print
      (list 'tenv-lookup-failure: (list 'missing: kind var) 'in: tenv)
    )
    (eopl:error  'lookup-variable-name-in-tenv)
  )
)

; Sym * Listof(Decl) -> Type
(define lookup-variable-name-in-decls
  (lambda (var-name decls0)
    (let loop ([decls decls0])
      (cond
        ((null? decls)
          (report-lookup-variable-in-decls-error var-name decls0)
        )
        ((eqv? var-name (decl->name (car decls)))
          (decl->type (car decls))
        )
        (else (loop (cdr decls)))
      )
    )
  )
)

; Sym * Listof(Decl)
(define report-lookup-variable-in-decls-error
  (lambda (var-name decls)
    (eopl:pretty-print
      (list 'lookup-variable-in-decls-failure:
        (list 'missing-variable var-name)
        'in:
        decls
      )
    )
    (eopl:error 'lookup-variable-in-decls)
  )
)

; Tenv * Sym -> Maybe(Type)
(define variable-name->maybe-binding-in-tenv
  (lambda (tenv search-name)
    (let recur ([tenv tenv])
      (cases t-environment tenv
        (empty-tenv () #f)
        (extend-tenv (name ty saved-tenv)
          (if (eqv? search-name name)
            ty
            (recur saved-tenv)
          )
        )
        (else (recur (tenv->saved-tenv tenv)))
      )
    )
  )
)

; Tenv * Sym -> Maybe(Iface)
(define module-name->maybe-binding-in-tenv
  (lambda (tenv search-name)
    (let recur ([tenv tenv])
      (cases t-environment tenv
        (empty-tenv () #f)
        (extend-tenv-with-module (name iface saved-tenv)
          (if (eqv? search-name name)
            iface
            (recur saved-tenv)
          )
        )
        (else (recur (tenv->saved-tenv tenv)))
      )
    )
  )
)

; Tenv -> Tenv
(define tenv->saved-tenv
  (lambda (tenv)
    (cases t-environment tenv
      (empty-tenv ()
        (eopl:error 'tenv->saved-tenv "tenv->saved-tenv called on empty-tenv")
      )
      (extend-tenv (name ty saved-tenv) saved-tenv)
      (extend-tenv-with-module (name iface saved-tenv) saved-tenv)
      (extend-tenv-with-type (name ty saved-tenv) saved-tenv)
    )
  )
)

; Type * Tenv -> ExpandedType
(define expand-type
  (lambda (ty tenv)
    (cases type ty
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (arg-type result-type)
        (proc-type
          (expand-type arg-type tenv)
          (expand-type result-type tenv)
        )
      )
      (named-type (name)
        (lookup-type-name-in-tenv tenv name)
      )
      (qualified-type (m-name t-name)
        (lookup-qualified-type-in-tenv m-name t-name tenv)
      )
    )
  )
)

; Tenv * Sym -> Type
(define lookup-type-name-in-tenv
  (lambda (tenv search-name)
    (cases t-environment tenv
      (empty-tenv () (report-tenv-lookup-failure-error 'type search-name tenv))
      (extend-tenv-with-type (saved-name saved-ty saved-tenv)
        (if (eqv? search-name saved-name)
          saved-ty
          (lookup-type-name-in-tenv saved-tenv search-name)
        )
      )
      (else (lookup-type-name-in-tenv (tenv->saved-tenv tenv) search-name))
    )
  )
)

; Sym * Sym * Tenv -> Type
(define lookup-qualified-type-in-tenv
  (lambda (m-name t-name tenv)
    (let ([maybe-iface (module-name->maybe-binding-in-tenv tenv m-name)])
      (if maybe-iface
        (cases interface maybe-iface
          (simple-iface (decls)
            (lookup-variable-name-in-decls t-name decls)
          )
          (proc-iface (param-name param-iface result-iface)
            (eopl:error 'lookup-qualified-var-in-tenv
              "can't retrieve variable from ~s take ~s from proc-iface" m-name t-name
            )
          )
        )
        (report-tenv-lookup-failure-error 'qualified-type t-name tenv)
      )
    )
  )
)

; Decl * Tenv -> Tenv
(define extend-tenv-with-decl
  (lambda (decl tenv)
    (cases declaration decl
      (val-decl (name ty) tenv)
      (transparent-type-decl (name ty)
        (extend-tenv-with-type name (expand-type ty tenv) tenv)
      )
      (opaque-type-decl (name)
        (extend-tenv-with-type name
          (qualified-type (fresh-module-name '%unknown) name)
          tenv
        )
      )
    )
  )
)
