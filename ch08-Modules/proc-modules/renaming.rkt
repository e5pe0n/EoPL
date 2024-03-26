#lang eopl

(require "utils.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; Iface * Sym * Sym -> Iface
(define rename-in-iface
  (lambda (m-type old new)
    (cases interface m-type
      (simple-iface (decls)
        (simple-iface
          (rename-in-decls decls old new)
        )
      )
      (proc-iface (param-name param-type result-type)
        (proc-iface param-name
          (rename-in-iface param-type old new)
          (if (eqv? param-name old)
            result-type
            (rename-in-iface result-type old new)
          )
        )
      )
    )
  )
)

; Listof(Decl) * Sym * Sym -> Listof(Decl)
(define rename-in-decls
  (lambda (decls old new)
    (if (null? decls)
      '()
      (let ([decl (car decls)] [decls (cdr decls)])
        (cases declaration decl
          (val-decl (name ty)
            (cons
              (val-decl name (rename-in-type ty old new))
              (rename-in-decls decls old new)
            )
          )
          (opaque-type-decl (name)
            (cons
              (opaque-type-decl name)
              (if (eqv? name old)
                decls
                (rename-in-decls decls old new)
              )
            )
          )
          (transparent-type-decl (name ty)
            (cons
              (transparent-type-decl name
                (rename-in-type ty old new)
              )
              (if (eqv? name old)
                decls
                (rename-in-decls decls old new)
              )
            )
          )
        )
      )
    )
  )
)

; Type * Sym * Sym -> Type
(define rename-in-type
  (lambda (ty old new)
    (cases type ty
      (named-type (id)
        (named-type (rename-name id old new))
      )
      (qualified-type (m-name name)
        (qualified-type (rename-name m-name) name)
      )
      (proc-type (t1 t2)
        (proc-type (rename-in-type t1 old new) (rename-in-type t2 old new))
      )
      (else ty)
    )
  )
)

; Sym * Sym * Sym -> Sym
(define rename-name
  (lambda (name old new)
    (if (eqv? name old) new name)
  )
)
