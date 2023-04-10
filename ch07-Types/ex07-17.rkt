#lang eopl

; Type * Tvar * Type -> Type
(define apply-one-subst
  (lambda (ty0 tvar ty1)
    (cases type ty0
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (arg-type result-type)
        (proc-type
          (apply-one-subst arg-type tvar t1)
          (apply-one-subst result-type tvar t1)
        )
      )
      (tvar-type (sn)
        (if (equal? ty0 tvar) ty1 ty0)
      )
    )
  )
)

; Type * Subst -> Type
(define apply-subst-to-type
  (lambda (ty subst)
    (cases type ty
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (t1 t2)
        (proc-type
          (apply-subst-to-type t1 subst)
          (apply-subst-to-type t2 subst)
        )
      )
      (tvar-type (sn)
        (let f ([subst subst] [r-subst '()])
          (if (null? subst)
            ty
            (let* ([p (car subst)] [oldlhs (car p)] [oldrhs (car p)])
              (if (equal? oldlhs ty)
                (let g ([r-subst r-subst] [t oldrhs])
                  (if (null? r-subst)
                    t
                    (let* ([q (car r-subst)] [lhs (car q)] [rhs (cdr q)])
                      (g (cdr r-subst) (apply-one-subst t lhs rhs))
                    )
                  )
                )
                (f (cdr subst) (cons (car subst) r-subst))
              )
            )
          )
        )
      )
    )
  )
)

; () -> Subst
(define empty-subst
  (lambda () '())
)

; Subst * Tvar * Type -> Subst
(define extend-subst
  (lambda (subst tvar ty)
    (cons (cons tvar ty) subst)
  )
)
