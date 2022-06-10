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
      (nameless-letrec-var-exp (n)
        (apply-nameless-env-rec nameless-env n)
      )
      (nameless-let-exp (exp1 body)
        (value-of body
          (extend-nameless-env (value-of exp1 nameless-env) nameless-env)
        )
      )
      (nameless-letrec-exp (p-body letrec-body)
        (value-of letrec-body
          (extend-nameless-env-rec p-body nameless-env)
        )
      )
      (nameless-proc-exp (body)
        (proc-val (procedure body nameless-env))
      )
      (nameless-unpack-exp (exp1 body)
        (value-of body
          (let f ([es (expval->list (value-of exp1 nameless-env))])
            (if (null? es)
              nameless-env
              (extend-nameless-env (any->expval (car es)) (f (cdr es)))
            )
          )
        )
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