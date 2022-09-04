#lang eopl

(require "data-structures.rkt")
(require "lang.rkt")

(provide (all-defined-out))

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

; Exp x Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (proc-exp (var body)
        (apply-cont cont
          (proc-val (procedure var body env))
        )
      )
      (letrec-exp (p-name b-var p-body letrec-body)
        (value-of/k
          letrec-body
          (extend-env-rec p-name b-var p-body env)
          cont
        )
      )
      (zero?-exp (exp1)
        (value-of/k exp1 env (zero1-cont cont))
      )
      (let-exp (var exp1 body)
        (value-of/k exp1 env
          (let-exp-cont var body env cont)
        )
      )
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (if-test-cont exp2 exp3 env cont)
        )
      )
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
      (call-exp (rator rand)
        (value-of/k rator env
          (rator-cont rand env cont)
        )
      )
    )
  )
)

; FinalAnswer = ExpVal

; Cont = ExpVal -> FinalAnswer

; () -> Cont
(define end-cont
  (lambda ()
    (lambda (val)
      (begin
        (eopl:printf "End of computation.~%")
        val
      )
    )
  )
)

; Cont -> Cont
(define zero1-cont
  (lambda (cont)
    (lambda (val)
      (apply-cont cont
        (bool-val
          (zero? (expval->num val))
        )
      )
    )
  )
)

; Var * Exp * Env * Cont -> Cont
(define let-exp-cont
  (lambda (var body env cont)
    (lambda (val)
      (value-of/k body (extend-env var val env) cont)
    )
  )
)

; Exp * Exp * Env * Cont -> Cont
(define if-test-cont
  (lambda (exp2 exp3 env cont)
    (lambda (val)
      (if (expval->bool val)
        (value-of/k exp2 env cont)
        (value-of/k exp3 env cont)
      )
    )
  )
)

; Operator * Exp * Env * Cont -> Cont
(define binary-op1-cont
  (lambda (op exp2 env cont)
    (lambda (val1)
      (value-of/k exp2 env
        (binary-op2-cont op val1 cont)
      )
    )
  )
)

(define binary-op2-cont
  (lambda (op val1 cont)
    (lambda (val2)
      (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
        (apply-cont cont
          (num-val (op num1 num2))
        )
      )
    )
  )
)

; Exp * Env * Cont -> Cont
(define rator-cont
  (lambda (rand env cont)
    (lambda (val1)
      (value-of/k rand env
        (rand-cont val1 cont)
      )
    )
  )
)

; ExpVal * Cont -> Cont
(define rand-cont
  (lambda (val1 cont)
    (lambda (val2)
      (let ([proc1 (expval->proc val1)])
        (apply-procedure/k proc1 val2 cont)
      )
    )
  )
)

; Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont v)
    (cont v)
  )
)

; Proc * ExpVal * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 val cont)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of/k body (extend-env var val saved-env) cont)
      )
    )
  )
)