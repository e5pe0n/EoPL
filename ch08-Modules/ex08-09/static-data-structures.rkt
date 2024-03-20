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
  (extend-tenv*
    (saved-vars (list-of symbol?))
    (saved-tys (list-of type?))
    (saved-tenv t-environment?)
  )
)

; () -> TEnv
(define init-tenv
  (lambda () (empty-tenv))
)

; Tenv * Sym * Listof(Module) -> Maybe(Type)
(define lookup-variable-name-in-tenv
  (lambda (tenv search-name m-deps)
    (let ([maybe-answer (variable-name->maybe-binding-in-tenv tenv search-name)])
      (if maybe-answer
        (cases type maybe-answer
          (iface-type (_)
            (if (any (lambda (x) (eqv? x search-name)) m-deps)
              maybe-answer
              (report-module-not-found-error search-name m-deps)
            )
          )
          (else maybe-answer)
        )
        (report-tenv-lookup-failure-error 'variable search-name tenv)
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
  (lambda (var-name decls)
    (let ([maybe-answer (variable-name->maybe-binding-in-decls decls var-name)])
      (if maybe-answer
        maybe-answer
        (report-lookup-variable-in-decls-error var-name decls)
      )
    )
  )
)

; Sym * Listof(Decl) -> ()
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

; Listof(Decl) * Sym -> Maybe(Type)
(define variable-name->maybe-binding-in-decls
  (lambda (decls0 search-name)
    (let loop ([decls decls0])
      (cond
        ((null? decls) #f)
        ((eqv? search-name (decl->name (car decls)))
          (decl->type (car decls))
        )
        (else (loop (cdr decls)))
      )
    )
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
        (extend-tenv* (names tys saved-tenv)
          (let loop ([names names] [tys tys])
            (if (null? names)
              (recur saved-tenv)
              (if (eqv? search-name (car names))
                (car tys)
                (loop (cdr names) (cdr tys))
              )
            )
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
      (extend-tenv* (names tys saved-tenv) saved-tenv)
    )
  )
)
