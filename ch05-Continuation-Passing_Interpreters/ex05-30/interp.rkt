#lang eopl

(require "data-structures.rkt")
(require "lang.rkt")
(require "utils.rkt")

(provide (all-defined-out))

; Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of/k exp1 (init-env) (end-cont))
      )
    )
  )
)

; FinalAnswer = ExpVal

(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont (cont continuation?))
  (let-exp-cont
    (var identifier?)
    (body expression?)
    (env environment?)
    (cont continuation?)
  )
  (if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (env environment?)
    (cont continuation?)
  )
  (binary-op1-cont
    (op procedure?)
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (binary-op2-cont
    (op procedure?)
    (val1 expval?)
    (saved-cont continuation?)
  )
  (rator-cont
    (rand expression?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (rand-cont
    (val1 expval?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
)

; Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
        (begin
          (eopl:printf "End of Computation.~%")
          val
        )
      )
      (zero1-cont (saved-cont)
        (apply-cont saved-cont
          (bool-val (zero? (expval->num val)))
        )
      )
      (let-exp-cont (var body saved-env saved-cont)
        (value-of/k body
          (extend-env var val saved-env)
          saved-cont
        )
      )
      (if-test-cont (exp2 exp3 saved-env saved-cont)
        (if (expval->bool val)
          (value-of/k exp2 saved-env saved-cont)
          (value-of/k exp3 saved-env saved-cont)
        )
      )
      (binary-op1-cont (op exp2 saved-env saved-cont)
        (value-of/k exp2 saved-env
          (binary-op2-cont op val saved-cont)
        )
      )
      (binary-op2-cont (op val1 saved-cont)
        (let ([num1 (expval->num val1)] [num2 (expval->num val)])
          (apply-cont saved-cont
            (num-val (op num1 num2))
          )
        )
      )
      (rator-cont (rand saved-env saved-cont)
        (value-of/k rand saved-env
          (rand-cont val saved-env saved-cont)
        )
      )
      (rand-cont (val1 saved-env saved-cont)
        (let ([proc1 (expval->proc val1)])
          (apply-procedure/k proc1 val saved-env saved-cont)
        )
      )
    )
  )
)

; Exp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (binary-op1-cont - exp2 env cont)
        )
      )
      (mul-exp (exp1 exp2)
        (value-of/k exp1 env
          (binary-op1-cont * exp2 env cont)
        )
      )
      (zero?-exp (exp1)
        (value-of/k exp1 env (zero1-cont cont))
      )
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (if-test-cont exp2 exp3 env cont)
        )
      )
      (let-exp (var exp1 body)
        (value-of/k exp1 env
          (let-exp-cont var body env cont)
        )
      )
      (letrec-exp (p-name b-var p-body letrec-body)
        (value-of/k
          letrec-body
          (extend-env-rec p-name b-var p-body env)
          cont
        )
      )
      (proc-exp (var body)
        (apply-cont cont
          (proc-val (procedure var body))
        )
      )
      (call-exp (rator rand)
        (value-of/k rator env
          (rator-cont rand env cont)
        )
      )
    )
  )
)

; Proc * ExpVal * Env * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 val env cont)
    (cases proc proc1
      (procedure (var body)
        (value-of/k body (extend-env var val env) cont)
      )
    )
  )
)
