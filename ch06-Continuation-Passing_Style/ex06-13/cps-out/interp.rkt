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
    (cases tf-expression exp
      (simple-exp->exp (simple)
        (apply-cont cont (value-of-simple-exp simple env))
      )
      (cps-let-exp (vars rhss body)
        (value-of/k body
          (extend-env* vars (map (lambda (rhs) (value-of-simple-exp rhs env)) rhss) env)
          cont
        )
      )
      (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
        (value-of/k letrec-body
          (extend-env-rec* p-names b-varss p-bodies env)
          cont
        )
      )
      (cps-if-exp (simple1 body1 body2)
        (if (cases expval (value-of-simple-exp simple1 env)
              (num-val (num) (not (= num 0)))
              (bool-val (bool) bool)
              (proc-val (proc1) #t)
              (list-val (list1) (not (null? list1)))
            )
          (value-of/k body1 env cont)
          (value-of/k body2 env cont)
        )
      )
      (cps-call-exp (rator rands)
        (let
          (
            [rator-proc (expval->proc (value-of-simple-exp rator env))]
            [rand-vals (map (lambda (simple) (value-of-simple-exp simple env)) rands)]
          )
          (apply-procedure/k rator-proc rand-vals cont)
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
            (expval->num (value-of-simple-exp simple1 env))
            (expval->num (value-of-simple-exp simple2 env))
          )
        )
      )
      (cps-mul-exp (simple1 simple2)
        (num-val
          (*
            (expval->num (value-of-simple-exp simple1 env))
            (expval->num (value-of-simple-exp simple2 env))
          )
        )
      )
      (cps-add1-exp (simple)
        (num-val
          (+ (expval->num (value-of-simple-exp simple env)) 1)
        )
      )
      (cps-zero?-exp (simple)
        (bool-val (zero? (expval->num (value-of-simple-exp simple env))))
      )
      (cps-number?-exp (simple)
        (bool-val
          (cases expval (value-of-simple-exp simple env)
            (num-val (num) #t)
            (else #f)
          )
        )
      )
      (cps-equal?-exp (simple1 simple2)
        (bool-val
          (=
            (expval->num (value-of-simple-exp simple1 env))
            (expval->num (value-of-simple-exp simple2 env))
          )
        )
      )
      (cps-less?-exp (simple1 simple2)
        (bool-val
          (<
            (expval->num (value-of-simple-exp simple1 env))
            (expval->num (value-of-simple-exp simple2 env))
          )
        )
      )
      (cps-greater?-exp (simple1 simple2)
        (bool-val
          (>
            (expval->num (value-of-simple-exp simple1 env))
            (expval->num (value-of-simple-exp simple2 env))
          )
        )
      )
      (cps-null?-exp (simple)
        (bool-val (null? (expval->list (value-of-simple-exp simple env))))
      )
      (cps-proc-exp (vars tf-exp)
        (proc-val (procedure vars tf-exp env))
      )
      (cps-emptylist-exp () (list-val '()))
      (cps-list-exp (simples)
        (list-val
          (map
            (lambda (simple)
              (expval->any (value-of-simple-exp simple env))
            )
            simples
          )
        )
      )
      (cps-car-exp (simple)
        (any->expval (car (expval->list (value-of-simple-exp simple env))))
      )
      (cps-cdr-exp (simple)
        (list-val (cdr (expval->list (value-of-simple-exp simple env))))
      )
      (cps-cons-exp (simple1 simple2)
        (list-val
          (cons
            (expval->any (value-of-simple-exp simple1 env))
            (expval->list (value-of-simple-exp simple2 env))
          )
        )
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
