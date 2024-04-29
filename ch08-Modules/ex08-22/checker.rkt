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
      (let-exp (var exp1 body)
        (let ([exp1-type (type-of exp1 tenv)])
          (type-of body
            (extend-tenv var exp1-type tenv)
          )
        )
      )
      (proc-exp (var var-type body)
        (let* (
            [expanded-var-type (expand-type var-type tenv)]
            [result-type (type-of body (extend-tenv var expanded-var-type tenv))]
          )
          (proc-type expanded-var-type result-type)
        )
      )
      (call-exp (rator rand)
        (let (
            [rator-type (type-of rator tenv)]
            [rand-type (type-of rand tenv)]
          )
          (cases type rator-type
            (proc-type (arg-type result-type)
              (begin
                (check-equal-type! arg-type rand-type rand)
                result-type
              )
            )
            (else (report-rator-not-a-proc-type rator-type rator))
          )
        )
      )
      (letrec-exp (p-result-type p-name b-var b-var-type p-body letrec-body)
        (let* (
            [
              tenv-for-letrec-body
              (extend-tenv p-name
                (expand-type (proc-type b-var-type p-result-type) tenv)
                tenv
              )
            ]
            [
              p-body-type
              (type-of p-body
                (extend-tenv b-var
                  (expand-type b-var-type tenv)
                  tenv-for-letrec-body
                )
              )
            ]
          )
          (check-equal-type! p-body-type p-result-type p-body)
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
