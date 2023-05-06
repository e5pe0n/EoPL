#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "static-data-structures.rkt")

(provide (all-defined-out))

; Type * Type * Exp -> Unspecified
(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (if (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp)
      0
    )
  )
)

; Listof(Type) * Listof(Type) * Listof(Exp) -> Unspecified
(define check-equal-types!
  (lambda (tys1 tys2 exps)
    (let loop ([tys1 tys1] [tys2 tys2] [exps exps])
      (if (null? tys1)
        0
        (begin
          (check-equal-type! (car tys1) (car tys2) (car exps))
          (loop (cdr tys1) (cdr tys2) (cdr exps))
        )
      )
    )
  )
)

; Type * Type * Exp -> Unspecified
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!
      "Types didn't match: ~s != ~s in~%~s"
      (type-to-external-form ty1)
      (type-to-external-form ty2)
      exp
    )
  )
)

; Exp * TEnv -> Type
(define type-of
  (lambda (exp tenv)
    (cases expression exp
      (const-exp (num) (int-type))
      (var-exp (var) (apply-tenv tenv var))
      (qualified-var-exp (m-name var)
        (lookup-qualified-var-in-tenv m-name var tenv)
      )
      (diff-exp (exp1 exp2)
        (let (
            [ty1 (type-of exp1 tenv)]
            [ty2 (type-of exp2 tenv)]
          )
          (check-equal-type! ty1 (int-type) exp1)
          (check-equal-type! ty2 (int-type) exp2)
          (int-type)
        )
      )
      (zero?-exp (exp1)
        (let ([ty1 (type-of exp1 tenv)])
          (check-equal-type! ty1 (int-type) exp1)
          (bool-type)
        )
      )
      (if-exp (exp1 exp2 exp3)
        (let (
            [ty1 (type-of exp1 tenv)]
            [ty2 (type-of exp2 tenv)]
            [ty3 (type-of exp3 tenv)]
          )
          (check-equal-type! ty1 (bool-type) exp1)
          (check-equal-type! ty2 ty3 exp)
          ty2
        )
      )
      (let-exp (vars exps body)
        (type-of body
          (extend-tenv* vars (map (lambda (exp1) (type-of exp1 tenv)) exps) tenv)
        )
      )
      (proc-exp (vars var-types body)
        (let ([result-type (type-of body (extend-tenv* vars var-types tenv))])
          (proc-type var-types result-type)
        )
      )
      (call-exp (rator rands)
        (let (
            [rator-type (type-of rator tenv)]
            [rand-types (map (lambda (rand) (type-of rand tenv)) rands)]
          )
          (cases type rator-type
            (proc-type (arg-types result-type)
              (begin
                (check-equal-types! arg-types rand-types rands)
                result-type
              )
            )
            (else (report-rator-not-a-proc-type rator-type rator))
          )
        )
      )
      (letrec-exp (p-result-types p-names b-varss b-var-typess p-bodies letrec-body)
        (let* (
            [tenv-for-letrec-body
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
                    (extend-tenv* b-vars b-var-types tenv-for-letrec-body)
                  )
                )
                b-varss b-var-typess p-bodies
              )
            ]
          )
          (check-equal-types! p-body-types p-result-types p-bodies)
          (type-of letrec-body tenv-for-letrec-body)
        )
      )
    )
  )
)

; Type * Exp -> Unspecified
(define report-rator-not-a-proc-type
  (lambda (rator-ty rator-exp)
    (eopl:error 'call-exp
      "rator must be a proc type;~%  rator-ty=~a~%  rator-exp=~a"
      (type-to-external-form rator-ty)
      (type-to-external-form rator-exp)
      exp
    )
  )
)
