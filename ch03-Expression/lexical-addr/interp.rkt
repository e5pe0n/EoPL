#lang eopl

(require "data-structures.rkt")
(require "lang.rkt")
(require "errors.rkt")

(provide (all-defined-out))

; () -> Env
(define init-nameless-env
  (lambda () (empty-nameless-env))
)

; Nameless-program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-nameless-env))
      )
    )
  )
)

; Nameless-exp x Nameless-env -> ExpVal
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (diff-exp (exp1 exp2)
        (num-val
          (-
            (expval->num (value-of exp1 nameless-env))
            (expval->num (value-of exp2 nameless-env))
          )
        )
      )
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 nameless-env))))
      )
      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 nameless-env))
          (value-of exp2 nameless-env)
          (value-of exp3 nameless-env)
        )
      )
      (call-exp (rator rand)
        (let
          (
            [proc1 (expval->proc (value-of rator nameless-env))]
            [arg (value-of rand nameless-env)]
          )
          (apply-procedure proc1 arg)
        )
      )
      (nameless-var-exp (n)
        (apply-nameless-env nameless-env n)
      )
      (nameless-let-exp (exp1 body)
        (value-of body
          (extend-nameless-env (value-of exp1 nameless-env) nameless-env)
        )
      )
      (nameless-proc-exp (body)
        (proc-val (procedure body nameless-env))
      )
      (else (report-invalid-translated-expression exp))
    )
  )
)

; Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (body saved-nameless-env)
        (value-of body (extend-nameless-env val saved-nameless-env))
      )
    )
  )
)