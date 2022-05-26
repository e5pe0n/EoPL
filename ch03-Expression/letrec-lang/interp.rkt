#lang eopl

(require "data-structures.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))
      )
    )
  )
)

; Exp x Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
        (num-val
          (-
            (expval->num (value-of exp1 env))
            (expval->num (value-of exp2 env))
          )
        )
      )
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 env))))
      )
      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env))
          (value-of exp2 env)
          (value-of exp3 env)
        )
      )
      (let-exp (var exp1 body)
        (value-of body
          (extend-env var (value-of exp1 env) env)
        )
      )
      (letrec-exp (p-name b-var p-body letrec-body)
        (value-of
          letrec-body
          (extend-env-rec p-name b-var p-body env)
        )
      )
      (proc-exp (var body)
        (proc-val (procedure var body env))
      )
      (call-exp (rator rand)
        (let
          (
            [proc1 (expval->proc (value-of rator env))]
            [arg (value-of rand env)]
          )
          (apply-procedure proc1 arg)
        )
      )
    )
  )
)

; Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body (extend-env var val saved-env))
      )
    )
  )
)