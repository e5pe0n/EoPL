#lang eopl

(require "data-structures.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (begin
          (initialize-store!)
          (value-of exp1 (init-env))
        )
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
          (extend-env* vars (map (lambda (exp1) (newref (value-of exp1 env))) exps) env)
        )
      )
      (letrec-exp (p-result-types p-names b-varss b-var-typess p-bodies letrec-body)
        (value-of
          letrec-body
          (extend-env-rec* p-names b-varss p-bodies env)
        )
      )
      (proc-exp (vars var-type body)
        (proc-val (procedure vars body env))
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
      (begin-exp (exp1 exps)
        (let ([vals (map (lambda (x) (value-of x env)) (cons exp1 exps))])
          (list-ref vals (- (length vals) 1))
        )
      )
      (assign-exp (var exp1)
        (begin
          (setref! (apply-env var) (value-of exp1 env))
          (num-val 27)
        )
      )
      (pair-exp (exp1 exp2)
        (pair-val (cons (value-of exp1 env) (value-of exp2 env)))
      )
      (unpair-exp (var1 var2 exp1 body)
        (let ([val (expval->pair (value-of exp1 env))])
          (value-of body
            (extend-env var2 (newref (cdr val))
              (extend-env var1 (newref (car val)) env)
            )
          )
        )
      )
      (list-exp (exp1 exps)
        (list-val
          (map (lambda (x) (expval->any (value-of x env))) (cons exp1 exps))
        )
      )
      (cons-exp (exp1 exp2)
        (list-val
          (cons (expval->any (value-of exp1 env)) (expval->any (value-of exp2 env)))
        )
      )
      (car-exp (exp1)
        (any->expval (car (expval->list (value-of exp1 env))))
      )
      (cdr-exp (exp1)
        (list-val (cdr (expval->list (value-of exp1 env))))
      )
      (null?-exp (exp1)
        (bool-val (null? (expval->list (value-of exp1 env))))
      )
      (emptylist-exp (ty)
        (list-val '())
      )
    )
  )
)

; Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of body (extend-env* vars (map (lambda (val) (newref val)) vals) saved-env))
      )
    )
  )
)
