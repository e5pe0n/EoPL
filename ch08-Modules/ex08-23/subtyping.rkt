#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "static-data-structures.rkt")
(require "renaming.rkt")

(provide (all-defined-out))

; Iface * Iface * Tenv -> Bool
(define <:-iface
  (lambda (iface1 iface2 tenv)
    (cases interface iface1
      (simple-iface (decls1)
        (cases interface iface2
          (simple-iface (decls2)
            (<:-decls decls1 decls2 tenv)
          )
          (proc-iface (param-name2 param-iface2 result-iface2) #f)
        )
      )
      (proc-iface (param-name1 param-iface1 result-iface1)
        (cases interface iface2
          (simple-iface (decls2) #f)
          (proc-iface (param-name2 param-iface2 result-iface2)
            (let ([new-name (fresh-module-name param-name1)])
              (let (
                  [result-iface1 (rename-in-iface result-iface1 param-name1 new-name)]
                  [result-iface2 (rename-in-iface result-iface2 param-name2 new-name)]
                )
                (and
                  (<:-iface param-iface2 param-iface1 tenv)
                  (<:-iface result-iface1 result-iface2
                    (extend-tenv-with-module new-name
                      (expand-iface new-name param-iface1 tenv)
                      tenv
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

; Listof(Decl) * Listof(Decl) * Tenv -> Bool
(define <:-decls
  (lambda (decls1 decls2 tenv)
    (cond
      ((null? decls2) #t)
      ((null? decls1) #f)
      (else
        (let (
            [name1 (decl->name (car decls1))]
            [name2 (decl->name (car decls2))]
          )
          (if (eqv? name1 name2)
            (and
              (<:-decl (car decls1) (car decls2) tenv)
              (<:-decls (cdr decls1) (cdr decls2)
                (extend-tenv-with-decl (car decls1) tenv)
              )
            )
            (<:-decls (cdr decls1) decls2
              (extend-tenv-with-decl (car decls1) tenv)
            )
          )
        )
      )
    )
  )
)

; Decl * Decl * Tenv -> Bool
(define <:-decl
  (lambda (decl1 decl2 tenv)
    (or
      (and
        (val-decl? decl1)
        (val-decl? decl2)
        (equiv-type? (decl->type decl1) (decl->type decl2) tenv)
      )
      (and
        (transparent-type-decl? decl1)
        (transparent-type-decl? decl2)
        (equiv-type? (decl->type decl1) (decl->type decl2) tenv)
      )
      (and
        (transparent-type-decl? decl1)
        (opaque-type-decl? decl2)
      )
      (and
        (opaque-type-decl? decl1)
        (opaque-type-decl? decl2)
      )
    )
  )
)

; Type * Type * Tenv -> Bool
(define equiv-type?
  (lambda (ty1 ty2 tenv)
    (equal?
      (expand-type ty1 tenv)
      (expand-type ty2 tenv)
    )
  )
)

; Sym * Iface * Tenv -> Iface
(define expand-iface
  (lambda (m-name iface tenv)
    (cases interface iface
      (simple-iface (decls)
        (simple-iface (expand-decls m-name decls tenv))
      )
      (proc-iface (param-name param-iface result-iface)
        iface
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
