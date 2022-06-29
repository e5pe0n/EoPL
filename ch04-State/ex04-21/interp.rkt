#lang eopl

(require "data-structures.rkt")
(require "lang.rkt")
(require "utils.rkt")

(provide (all-defined-out))

; Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
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
      (var-exp (var) (deref (apply-env env var)))
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
      (let-exp (vars exps body)
        (value-of body
          (extend-env vars (map (lambda (x) (newref (value-of x env))) exps) env)
        )
      )
      (letrec-exp (p-names b-vars-list p-bodies letrec-body)
        (value-of
          letrec-body
          (extend-env-rec p-names b-vars-list p-bodies env)
        )
      )
      (proc-exp (vars body)
        (proc-val (procedure vars body env))
      )
      (call-exp (rator rands)
        (let
          (
            [proc1 (expval->proc (value-of rator env))]
            [args (map (lambda (x) (value-of x env)) rands)]
          )
          (apply-procedure proc1 args)
        )
      )
      (newref-exp (exp1)
        (ref-val (newref (value-of exp1 env)))
      )
      (deref-exp (exp1)
        (deref (expval->ref (value-of exp1 env)))
      )
      (setref-exp (exp1 exp2)
        (begin
          (setref!
            (expval->ref (value-of exp1 env))
            (value-of exp2 env)
          )
          (num-val 23)
        )
      )
      (assign-exp (var exp1)
        (begin
          (setref!
            (apply-env env var)
            (value-of exp1 env)
          )
          (num-val 27)
        )
      )
      (begin-exp (exp1 exps)
        (let ([vals (map (lambda (x) (value-of x env)) (cons exp1 exps))])
          (list-ref vals (- (length vals) 1))
        )
      )
      (setdynamic-exp (var exp1 body)
        (let* ([ref (apply-env env var)] [org (deref ref)])
          (begin
            (setref! ref (value-of exp1 env))
            (let ([res (value-of body env)])
              (setref! ref org)
              res
            )
          )
        )
      )
    )
  )
)

; Proc * Listof(ExpVal) -> ExpVal
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of body
          (extend-env vars (map (lambda (x) (newref x)) vals) saved-env)
        )
      )
    )
  )
)