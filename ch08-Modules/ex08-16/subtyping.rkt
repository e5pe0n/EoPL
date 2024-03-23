#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "static-data-structures.rkt")

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
