#lang eopl

(require "data-structures.rkt")
(require "lang.rkt")
(require "utils.rkt")

(provide (all-defined-out))

; Program -> ExpVal
(define result-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (stmt1)
        (result-of stmt1 (init-env))
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
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 env))))
      )
      (not-exp (exp1)
        (bool-val (not (expval->bool (value-of exp1 env))))
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
    )
  )
)

(define result-of
  (lambda (stmt env)
    (cases statement stmt
      (assign-stmt (var exp1)
        (begin
          (setref!
            (apply-env env var)
            (value-of exp1 env)
          )
          (num-val 27)
        )
      )
      (print-stmt (exp1)
        (begin
          (print (value-of exp1 env))
          (num-val 27)
        )
      )
      (begin-stmt (stmts)
        (if (null? stmts)
          (num-val 27)
          (begin
            (result-of (car stmts) env)
            (result-of (begin-stmt (cdr stmts)) env)
          )
        )
      )
      (if-stmt (exp1 stmt1 stmt2)
        (if (expval->bool (value-of exp1 env))
          (result-of stmt1 env)
          (result-of stmt2 env)
        )
      )
      (while-stmt (exp1 stmt1)
        (if (expval->bool (value-of exp1 env))
          (result-of (begin-stmt (list stmt1 (while-stmt exp1 stmt1))) env)
          (num-val 27)
        )
      )
      (var-stmt (vars stmt1)
        (result-of stmt1
          (extend-env vars (map (lambda (x) (newref (num-val 0))) vars) env)
        )
      )
      (read-stmt (var)
        (result-of (assign-stmt var (const-exp (read))) env)
      )
      (do-while-stmt (stmt1 exp1)
        (begin
          (result-of stmt1 env)
          (result-of (while-stmt exp1 stmt1) env)
        )
      )
    )
  )
)

; Proc * ExpVal -> ExpVal
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