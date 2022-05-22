#lang eopl

(require "env.rkt")
(require "expval.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; () -> Env
(define init-env
  (lambda ()
    (extend-env 'i (num-val 1)
      (extend-env 'v (num-val 5)
        (extend-env 'x (num-val 10)
          (empty-env)
        )
      )
    )
  )
)

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

; Exp x Env -> ExpVal(Number)
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
      (if-exp (b-exp1 exp1 exp2)
        (if (expval->bool (value-of-bool-exp b-exp1 env))
          (value-of exp1 env)
          (value-of exp2 env)
        )
      )
      (let-exp (var exp1 body)
        (value-of body
          (extend-env var (value-of exp1 env) env)
        )
      )
    )
  )
)

; Exp * Env -> ExpVal(Bool)
(define value-of-bool-exp
  (lambda (exp env)
    (cases bool-exp exp
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 env))))
      )
      (equal?-exp (exp1 exp2)
        (bool-val
          (=
            (expval->num (value-of exp1 env))
            (expval->num (value-of exp2 env))
          )
        )
      )
      (greater?-exp (exp1 exp2)
        (bool-val
          (>
            (expval->num (value-of exp1 env))
            (expval->num (value-of exp2 env))
          )
        )
      )
      (less?-exp (exp1 exp2)
        (bool-val
          (<
            (expval->num (value-of exp1 env))
            (expval->num (value-of exp2 env))
          )
        )
      )
    )
  )
)
