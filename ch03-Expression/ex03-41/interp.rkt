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
      (cond-exp (lhs-list rhs-list)
        (let f ([lhss lhs-list] [rhss rhs-list])
          (if (null? lhss)
            (report-no-corresponding-condition 'value-of_cond-exp)
            (if (expval->bool (value-of (car lhss) nameless-env))
              (value-of (car rhss) nameless-env)
              (f (cdr lhss) (cdr rhss))
            )
          )
        )
      )
      (cons-exp (exp1 exp2)
        (list-val
          (cons
            (expval->any (value-of exp1 nameless-env))
            (expval->any (value-of exp2 nameless-env))
          )
        )
      )
      (emptylist-exp () (list-val '()))
      (call-exp (rator rands)
        (let
          (
            [proc1 (expval->proc (value-of rator nameless-env))]
            [args (map (lambda (x) (value-of x nameless-env)) rands)]
          )
          (apply-procedure proc1 args)
        )
      )
      (nameless-var-exp (i j)
        (apply-nameless-env nameless-env (cons i  j))
      )
      (nameless-letrec-var-exp (i j)
        (apply-nameless-env-rec nameless-env (cons i j))
      )
      (nameless-let-exp (exps body)
        (value-of body
          (extend-nameless-env (map (lambda (x) (value-of x nameless-env)) exps) nameless-env)
        )
      )
      (nameless-letrec-exp (p-bodys letrec-body)
        (value-of letrec-body
          (extend-nameless-env-rec p-bodys nameless-env)
        )
      )
      (nameless-proc-exp (body)
        (proc-val (procedure body nameless-env))
      )
      (nameless-unpack-exp (exp1 body)
        (value-of body (extend-nameless-env (map (lambda (x) (any->expval x)) (expval->list (value-of exp1 nameless-env))) nameless-env))
      )
      (else (report-invalid-translated-expression exp))
    )
  )
)

; Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (body saved-nameless-env)
        (value-of body (extend-nameless-env vals saved-nameless-env))
      )
    )
  )
)