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
        (value-of/k exp1 (init-env) (end-cont))
      )
    )
  )
)

(define-datatype continuation continuation?
  (end-cont)
)

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
        (begin
          (println "End of Computation.")
          val
        )
      )
    )
  )
)


; TfExp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (simple-exp->exp (simple)
        (apply-cont cont (value-of-simple-exp simple env))
      )
      (let-exp (var rhs body)
        (let ([val (value-of-simple-exp rhs env)])
          (value-of/k body
            (extend-env var val env)
            cont
          )
        )
      )
      (letrec-exp (p-names b-varss p-bodies letrec-body)
        (value-of/k letrec-body
          (extend-env-rec* p-names b-varss p-bodies env)
          cont
        )
      )
      (if-exp (simple1 body1 body2)
        (if (expval->bool (value-of-simple-expression simple1 env))
          (value-of/k body1 env cont)
          (value-of/k body2 env cont)
        )
      )
      (call-exp (rator rands)
        (let
          (
            [rator-proc (expval->proc (value-of-simple-exp rator env))]
            [rand-vals (map (lambda (simple) (value-of-simple-exp simple env)) rands)]
          )
          (apply-procedure/k rator-cont rand-vals cont)
        )
      )
    )
  )
)

; SimpleExp * Env -> FinalAnswer
(define value-of-simple-exp
  (lambda (exp env)
    (cases simple-expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (cps-diff-exp (simple1 simple2)
        (num-val
          (-
            (expval->num (value-of-simple-exp simple1 env cont))
            (expval->num (value-of-simple-exp simple2 env cont))
          )
        )
      )
      (cps-zero?-exp (simple)
        (bool-val (value-of-simple-exp simple env))
      )
      (cps-proc-exp (vars tf-exp)
        (proc-val (procedure vars tf-exp env))
      )
    )
  )
)

; Proc * ExpVal * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 vals cont)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of/k body (extend-env* vars vals saved-env) cont)
      )
    )
  )
)
