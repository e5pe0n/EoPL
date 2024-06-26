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
      (let-exp (var exp1 body)
        (value-of body
          (extend-env var (newref (value-of exp1 env)) env)
        )
      )
      (letrec-exp (p-names b-vars p-bodies letrec-body)
        (value-of
          letrec-body
          (extend-env-rec p-names b-vars p-bodies env)
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
      (newpair-exp (exp1 exp2)
        (mutpair-val
          (make-pair
            (value-of exp1 env)
            (value-of exp2 env)
          )
        )
      )
      (left-exp (exp1)
        (left
          (expval->mutpair (value-of exp1 env))
        )
      )
      (right-exp (exp1)
        (right
          (expval->mutpair (value-of exp1 env))
        )
      )
      (setleft-exp (exp1 exp2)
        (let ([val1 (value-of exp1 env)] [val2 (value-of exp2 env)])
          (let ([p (expval->mutpair val1)])
            (begin
              (setleft p val2)
              (num-val 82)
            )
          )
        )
      )
      (setright-exp (exp1 exp2)
        (let ([val1 (value-of exp1 env)] [val2 (value-of exp2 env)])
          (let ([p (expval->mutpair val1)])
            (begin
              (setright p val2)
              (num-val 83)
            )
          )
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
        (value-of body (extend-env var (newref val) saved-env))
      )
    )
  )
)