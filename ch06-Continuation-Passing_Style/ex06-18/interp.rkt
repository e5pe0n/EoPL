#lang eopl

(require "utils.rkt")
(require "data-structures.rkt")
(require "lang.rkt")
(require "errors.rkt")

(provide (all-defined-out))

; FinalAnswer = ExpVal
; Program -> FinalAnswer
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of/k exp1 (init-env))
      )
    )
  )
)


; TfExp * Env -> FinalAnswer
(define value-of/k
  (lambda (exp env)
    (cases tf-expression exp
      (simple-exp->exp (simple)
        (value-of-simple-exp simple env)
      )
      (cps-let-exp (vars rhss body)
        (value-of/k body
          (extend-env* vars (map (lambda (rhs) (value-of-simple-exp rhs env)) rhss) env)
        )
      )
      (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
        (value-of/k letrec-body
          (extend-env-rec* p-names b-varss p-bodies env)
        )
      )
      (cps-if-exp (simple1 body1 body2)
        (if (cases expval (value-of-simple-exp simple1 env)
              (num-val (num) (not (= num 0)))
              (bool-val (bool) bool)
              (proc-val (proc1) #t)
              (list-val (list1) (not (null? list1)))
            )
          (value-of/k body1 env)
          (value-of/k body2 env)
        )
      )
      (cps-call-exp (rator rands)
        (let
          (
            [rator-proc (expval->proc (value-of-simple-exp rator env))]
            [rand-vals (map (lambda (simple) (value-of-simple-exp simple env)) rands)]
          )
          (apply-procedure/k rator-proc rand-vals)
        )
      )
    )
  )
)

; SimpleExp * Env -> FinalAnswer
(define value-of-simple-exp
  (lambda (exp env)
    (cases simple-expression exp
      (const-or-var-exp->exp (const-or-var)
        (value-of-const-or-var-exp const-or-var env)
      )
      (cps-diff-exp (const-or-var1 const-or-var2)
        (num-val
          (-
            (expval->num (value-of-const-or-var-exp const-or-var1 env))
            (expval->num (value-of-const-or-var-exp const-or-var2 env))
          )
        )
      )
      (cps-mul-exp (const-or-var1 const-or-var2)
        (num-val
          (*
            (expval->num (value-of-const-or-var-exp const-or-var1 env))
            (expval->num (value-of-const-or-var-exp const-or-var2 env))
          )
        )
      )
      (cps-add1-exp (const-or-var)
        (num-val
          (+ (expval->num (value-of-const-or-var-exp const-or-var env)) 1)
        )
      )
      (cps-zero?-exp (const-or-var)
        (bool-val (zero? (expval->num (value-of-const-or-var-exp const-or-var env))))
      )
      (cps-number?-exp (const-or-var)
        (bool-val
          (cases expval (value-of-const-or-var-exp const-or-var env)
            (num-val (num) #t)
            (else #f)
          )
        )
      )
      (cps-equal?-exp (const-or-var1 const-or-var2)
        (bool-val
          (=
            (expval->num (value-of-const-or-var-exp const-or-var1 env))
            (expval->num (value-of-const-or-var-exp const-or-var2 env))
          )
        )
      )
      (cps-less?-exp (const-or-var1 const-or-var2)
        (bool-val
          (<
            (expval->num (value-of-const-or-var-exp const-or-var1 env))
            (expval->num (value-of-const-or-var-exp const-or-var2 env))
          )
        )
      )
      (cps-greater?-exp (const-or-var1 const-or-var2)
        (bool-val
          (>
            (expval->num (value-of-const-or-var-exp const-or-var1 env))
            (expval->num (value-of-const-or-var-exp const-or-var2 env))
          )
        )
      )
      (cps-null?-exp (const-or-var)
        (bool-val (null? (expval->list (value-of-const-or-var-exp const-or-var env))))
      )
      (cps-proc-exp (vars tf-exp)
        (proc-val (procedure vars tf-exp env))
      )
      (cps-emptylist-exp () (list-val '()))
      (cps-list-exp (const-or-vars)
        (list-val
          (map
            (lambda (const-or-var)
              (expval->any (value-of-const-or-var-exp const-or-var env))
            )
            const-or-vars
          )
        )
      )
      (cps-car-exp (const-or-var)
        (any->expval (car (expval->list (value-of-const-or-var-exp const-or-var env))))
      )
      (cps-cdr-exp (const-or-var)
        (list-val (cdr (expval->list (value-of-const-or-var-exp const-or-var env))))
      )
      (cps-cons-exp (const-or-var1 const-or-var2)
        (list-val
          (cons
            (expval->any (value-of-const-or-var-exp const-or-var1 env))
            (expval->list (value-of-const-or-var-exp const-or-var2 env))
          )
        )
      )
    )
  )
)

; ConstOrVarExp * Env -> FinalAnswer
(define value-of-const-or-var-exp
  (lambda (exp env)
    (cases const-or-var-expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
    )
  )
)


; Proc * ExpVal -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of/k body (extend-env* vars vals saved-env))
      )
    )
  )
)
