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
      (let-exp (var exp1 vars exps body)
        (let ([vars (cons var vars)] [exps (cons exp1 exps)])
          (type-of body
            (extend-tenv* vars
              (map (lambda (x) (type-of x tenv)) exps)
              tenv
            )
          )
        )
      )
      (proc-exp (vars var-types body)
        (let* (
            [expanded-var-types (map (lambda (x) (expand-type x tenv)) var-types)]
            [result-type (type-of body (extend-tenv* vars expanded-var-types tenv))]
          )
          (proc-type expanded-var-types result-type)
        )
      )
      (call-exp (rator rands)
        (let (
            [rator-type (type-of rator tenv)]
            [rand-types (map (lambda (x) (type-of x tenv)) rands)]
          )
          (cases type rator-type
            (proc-type (arg-types result-type)
              (begin
                (map
                  (lambda (x) (check-equal-type! (car x) (cadr x) rands))
                  (zip (list arg-types rand-types))
                )
                result-type
              )
            )
            (else (report-rator-not-a-proc-type rator-type rator))
          )
        )
      )
      (letrec-exp (
          p-result-type p-name b-vars b-var-types p-body
          p-result-types p-names b-varss b-var-typess p-bodys letrec-body
        )
        (let* (
            [p-result-types (cons p-result-type p-result-types)]
            [p-names (cons p-name p-names)]
            [b-varss (cons b-vars b-varss)]
            [b-var-typess (cons b-var-types b-var-typess)]
            [p-bodys (cons p-body p-bodys)]
            [
              tenv-for-letrec-body
              (extend-tenv* p-names
                (map
                  (lambda (p)
                    (let ([p-result-type (car p)] [p-name (cadr p)] [b-var-types (caddr p)])
                      (expand-type (proc-type b-var-types p-result-type))
                    )
                  )
                  (zip (list p-result-types p-names b-var-typess))
                )
                tenv
              )
            ]
            [
              p-body-types
              (map
                (lambda (p)
                  (let ([p-name (car p)] [b-vars (cadr p)] [b-var-types (caddr p)] [p-body (cadddr p)])
                    (type-of p-body
                      (extend-tenv* b-vars
                        (map (lambda (t) (expand-type t tenv)) b-var-types)
                        tenv-for-letrec-body
                      )
                    )
                  )
                )
                (zip (list p-names b-varss b-var-typess p-bodys))
              )
            ]
          )
          (map
            (lambda (x)
              (let ([p-body-type (car x)] [p-result-type (cadr x)])
                (check-equal-type! p-body-type p-result-type exp)
              )
            )
            (zip (list p-body-types p-result-types))
          )
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
      rator-exp
    )
  )
)
