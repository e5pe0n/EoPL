#lang eopl

(require "utils.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; Iface * IFace * Tenv -> Bool
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
    (all
      (lambda (d1)
        (let f ([ds2 decls2])
          (if (null? ds2)
            #f
            (let (
                [name1 (decl->name d1)]
                [name2 (decl->name (car ds2))]
              )
              (if (eqv? name1 name2)
                (equal? (decl->type d1) (decl->type (car ds2)))
                (f (cdr ds2))
              )
            )
          )
        )
      )
      decls1
    )
  )
)
