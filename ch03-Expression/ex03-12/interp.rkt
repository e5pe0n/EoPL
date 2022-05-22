#lang eopl

(require "env.rkt")
(require "expval.rkt")
(require "lang.rkt")
(require "errors.rkt")

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

; Exp x Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (minus-exp (exp1)
        (num-val (- 0 (expval->num (value-of exp1 env))))
      )
      (diff-exp (exp1 exp2)
        (num-val
          (-
            (expval->num (value-of exp1 env))
            (expval->num (value-of exp2 env))
          )
        )
      )
      (add-exp (exp1 exp2)
        (num-val
          (+
            (expval->num (value-of exp1 env))
            (expval->num (value-of exp2 env))
          )
        )
      )
      (mul-exp (exp1 exp2)
        (num-val
          (*
            (expval->num (value-of exp1 env))
            (expval->num (value-of exp2 env))
          )
        )
      )
      (quo-exp (exp1 exp2)
        (num-val
          (quotient
            (expval->num (value-of exp1 env))
            (expval->num (value-of exp2 env))
          )
        )
      )
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 env))))
      )
      (equal?-exp (exp1 exp2)
        (bool-val
          (eq?
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
      (list-exp (list1)
        (list-val
          (let f ([lst1 list1])
            (if (null? lst1)
              '()
              (cons
                (expval->any (value-of (car lst1) env))
                (f (cdr lst1))
              )
            )
          )
        )
      )
      (emptylist-exp () (list-val '()))
      (cons-exp (exp1 exp2)
        (list-val
          (cons
            (expval->any (value-of exp1 env))
            (expval->any (value-of exp2 env))
          )
        )
      )
      (car-exp (exp1)
        (any->expval
          (car (expval->list (value-of exp1 env)))
        )
      )
      (cdr-exp (exp1)
        (any->expval
          (cdr (expval->list (value-of exp1 env)))
        )
      )
      (null?-exp (exp1)
        (bool-val
          (null? (expval->list (value-of exp1 env)))
        )
      )
      (cond-exp (lhs-list rhs-list)
        (let f ([lhss lhs-list] [rhss rhs-list])
          (if (null? lhss)
            (report-no-corresponding-condition 'value-of_cond-exp)
            (if (expval->bool (value-of (car lhss) env))
              (value-of (car rhss) env)
              (f (cdr lhss) (cdr rhss))
            )
          )
        )
      )
    )
  )
)