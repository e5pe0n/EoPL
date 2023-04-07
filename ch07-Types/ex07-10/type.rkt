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
      (let-exp (vars exps body)
        (let ([exp-types (map (lambda (exp1) (type-of exp1 tenv)) exps)])
          (type-of body
            (extend-tenv* vars exp-types tenv)
          )
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
              (extend-tenv* p-names (map (lambda (b-var-types p-result-type) (proc-type b-var-types p-result-type)) b-var-typess p-result-types) tenv
              )
            ]
            [p-body-types
              (map
                (lambda (p-body b-vars b-var-types)
                  (type-of p-body (extend-tenv* b-vars b-var-types tenv-for-letrec-body)
                  )
                )
                p-bodies b-varss b-var-typess
              )
            ]
          )
          (map
            (lambda (p-body-type p-result-type p-body)
              (check-equal-type! p-body-type p-result-type p-body)
            )
            p-body-types p-result-types p-bodies
          )
          (type-of letrec-body tenv-for-letrec-body)
        )
      )
      (newref-exp (exp1) (ref-type (type-of exp1 tenv)))
      (deref-exp (exp1)
        (let ([ty (type-of exp1 tenv)])
          (cases type ty
            (ref-type (t) t)
            (else (report-exp-not-a-ref-type ty exp1))
          )
        )
      )
      (setref-exp (exp1 exp2) (void-type))
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
  (extend-tenv*
    (saved-var (list-of symbol?))
    (saved-ty (list-of type?))
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
      (extend-tenv* (saved-vars saved-tys saved-tenv)
        (let ([index (list-index (lambda (x) (eqv? x search-var)) saved-vars)])
          (if index
            (list-ref saved-tys index)
            (apply-tenv saved-tenv search-var)
          )
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
(define check-equal-types!
  (lambda (tys1 tys2 exp)
    (let f ([tys1 tys1] [tys2 tys2])
      (if (not (equal? (car tys1) (car tys2)))
        (report-unequal-types tys1 tys2 exp)
        0
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

; Type * Exp -> Unspecified
(define report-rator-not-a-proc-type
  (lambda (rator-ty rator-exp)
    (eopl:error 'call-exp
      "rator must be a proc type;~%  rator-ty=~s~%  rator-exp=~s"
      (type-to-external-form rator-ty)
      rator-exp
    )
  )
)

; Type * Exp -> Unspecified
(define report-exp-not-a-ref-type
  (lambda (ty exp)
    (eopl:error 'call-exp
      "rator must be a proc type;~%  ty=~s~%  exp=~s"
      (type-to-external-form ty)
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
      (proc-type (arg-types result-type)
        (append
          (list (map (lambda (t) (type-to-external-form t)) arg-types))
          (list '-> (type-to-external-form result-type))
        )
      )
      (ref-type (t) (list 'refto (type-to-external-form t)))
      (void-type () 'void)
    )
  )
)
