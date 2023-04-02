#lang eopl

(require "data-structures.rkt")
(require "utils.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; Program -> Type
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1) (type-of exp1 (init-tenv)))
    )
  )
)

; Exp * TEnv -> Type
(define type-of
  (lambda (exp tenv)
    (cases expression exp
      (const-exp (num) (int-type))
      (var-exp (var) (apply-tenv tenv var))
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
        (let ([result-type (type-of body (extend-tenv var var-type tenv))])
          (proc-type var-type result-type)
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
            [tenv-for-letrec-body (extend-tenv p-name (proc-type b-var-type p-result-type) tenv)]
            [p-body-type (type-of p-body (extend-tenv b-var b-var-type tenv-for-letrec-body))]
          )
          (check-equal-type! p-body-type p-result-type p-body)
          (type-of letrec-body tenv-for-letrec-body)
        )
      )
    )
  )
)

(define-datatype t-environment t-environment?
  (empty-tenv)
  (extend-tenv
    (saved-var symbol?)
    (saved-ty type?)
    (saved-tenv t-environment?)
  )
)

; () -> TEnv
(define init-tenv
  (lambda () (empty-tenv))
)

; TEnv * Var -> Type
(define apply-tenv
  (lambda (tenv search-var)
    (cases t-environment tenv
      (empty-tenv () (report-no-binding-found 'apply-tenv search-var))
      (extend-tenv (saved-var saved-ty saved-tenv)
        (if (eqv? search-var saved-var)
          saved-ty
          (apply-tenv saved-tenv search-var)
        )
      )
    )
  )
)

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

; Type -> List
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (proc-type (arg-type result-type)
        (list
          (type-to-external-form arg-type)
          '->
          (type-to-external-form result-type)
        )
      )
    )
  )
)
