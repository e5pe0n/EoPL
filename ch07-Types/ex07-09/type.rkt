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
        (begin
          (check-equal-type! (type-of exp1 tenv) (bool-type) exp1)
          (let ([ty2 (type-of exp2 tenv)])
            (check-equal-type! ty2 (type-of exp3 tenv) exp)
            ty2
          )
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
      (begin-exp (exp1 exps)
        (let ([exps (cons exp1 exps)])
          (type-of (list-ref exps (- (length exps) 1)) tenv)
        )
      )
      (assign-exp (var exp1)
        (check-equal-type! (apply-tenv tenv var) (type-of exp1 tenv) exp)
      )
      (pair-exp (exp1 exp2)
        (pair-type (type-of exp1 tenv) (type-of exp2 tenv))
      )
      (unpair-exp (var1 var2 exp1 body)
        (let ([ty (type-of exp1 tenv)])
          (cases type ty
            (pair-type (ty1 ty2)
              (type-of body
                (extend-tenv var2 ty2
                  (extend-tenv var1 ty1 tenv)
                )
              )
            )
            (else (report-exp-not-a-pair-type ty exp1))
          )
        )
      )
      (list-exp (exp1 exps)
        (let ([t (type-of exp1 tenv)])
          (begin
            (map (lambda (x) (check-equal-type! t (type-of x tenv) exp)) exps)
            (list-type t)
          )
        )
      )
      (cons-exp (exp1 exp2)
        (let ([ty1 (type-of exp1 tenv)] [ty2 (type-of exp2 tenv)])
          (let (
              [t2 (cases type ty2
                    (list-type (t) t)
                    (else (report-exp-not-a-list-type 'cons-exp ty2 exp2))
                  )
              ]
            )
            (check-equal-type! ty1 t2 exp)
            ty2
          )
        )
      )
      (car-exp (exp1)
        (let ([ty (type-of exp1 tenv)])
          (cases type ty
            (list-type (t) t)
            (else (report-exp-not-a-list-type 'car-exp ty exp1))
          )
        )
      )
      (cdr-exp (exp1)
        (let ([ty (type-of exp1 tenv)])
          (cases type ty
            (list-type (t) ty)
            (else (report-exp-not-a-list-type 'cdr-exp ty exp1))
          )
        )
      )
      (null?-exp (exp1)
        (let ([ty (type-of exp1 tenv)])
          (cases type ty
            (list-type (t) (bool-type))
            (else (report-exp-not-a-list-type 'null?-exp ty exp1))
          )
        )
      )
      (emptylist-exp (ty)
        (list-type ty)
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
      (type-to-external-form rator-ty) rator-exp exp
    )
  )
)

; Type * Exp -> Unspecified
(define report-exp-not-a-pair-type
  (lambda (ty exp)
    (eopl:error 'unpair-exp
      "expression must be a pair type;~%  received type=~s~%  received expression=~s"
      (type-to-external-form ty) exp
    )
  )
)

; Type * Exp -> Unspecified
(define report-exp-not-a-list-type
  (lambda (err-name ty exp)
    (eopl:error err-name
      "expression must be a list type;~%  received type=~s~%  received expression=~s"
      (type-to-external-form ty) exp
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
      (pair-type (ty1 ty2)
        (list 'pairof (type-to-external-form ty1) (type-to-external-form ty2))
      )
      (list-type (ty)
        (list 'listof (type-to-external-form ty))
      )
    )
  )
)
