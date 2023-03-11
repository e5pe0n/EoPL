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
        (value-of exp1 (init-env))
      )
    )
  )
)

; TfExp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases inp-expression exp
      (const-exp (num) (num-val num))
      (diff-exp (exp1 exp2)
        (num-val
          (-
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
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 env))))
      )
      (if-exp (exp1 exp2 exp3)
        (if (cases expval (value-of exp1 env)
              (num-val (num) (not (= num 0)))
              (bool-val (bool) bool)
              (proc-val (proc1) #t)
              (list-val (list1) (not (null? list1)))
            )
          (value-of exp2 env)
          (value-of exp3 env)
        )
      )
      (var-exp (var) (apply-env env var))
      (let-exp (vars exps body)
        (value-of body
          (extend-env* vars
            (map (lambda (x) (value-of x env)) exps)
            env
          )
        )
      )
      (letrec-exp (p-names b-varss p-bodies letrec-body)
        (value-of letrec-body
          (extend-env-rec* p-names b-varss p-bodies env)
        )
      )
      (proc-exp (vars exp1)
        (proc-val (procedure vars exp1 env))
      )
      (call-exp (rator rands)
        (let
          (
            [proc1 (expval->proc (value-of rator env))]
            [args (map (lambda (rand) (value-of rand env)) rands)]
          )
          (apply-procedure proc1 args)
        )
      )

      (emptylist-exp ()
        (list-val '())
      )
      (null?-exp (exp1)
        (bool-val (null? (expval->list (value-of exp1 env))))
      )
      (number?-exp (exp1)
        (bool-val
          (cases expval (value-of exp1 env)
            (num-val (num) #t)
            (else #f)
          )
        )
      )
      (equal?-exp (exp1 exp2)
        (bool-val
          (=
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
      (greater?-exp (exp1 exp2)
        (bool-val
          (>
            (expval->num (value-of exp1 env))
            (expval->num (value-of exp2 env))
          )
        )
      )
      (list-exp (exps)
        (list-val
          (let f ([es exps])
            (if (null? es)
              '()
              (cons
                (expval->any (value-of (car es) env))
                (f (cdr es))
              )
            )
          )
        )
      )
      (cons-exp (exp1 exp2)
        (list-val
          (cons
            (expval->any (value-of exp1 env))
            (expval->any (value-of exp2 env))
          )
        )
      )
      (car-exp (exp1)
        (any->expval (car (expval->list (value-of exp1 env))))
      )
      (cdr-exp (exp1)
        (any->expval (cdr (expval->list (value-of exp1 env))))
      )
      (add1-exp (exp1)
        (num-val (+ (expval->num (value-of exp1 env)) 1))
      )
    )
  )
)

; Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of body (extend-env* vars vals saved-env))
      )
    )
  )
)
