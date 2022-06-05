#lang eopl

(require "data-structures.rkt")
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

; Exp x Env -> ExpVal
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
      (letrec-exp (p-names b-vars p-bodys letrec-body)
        (value-of
          letrec-body
          (extend-env-rec p-names b-vars p-bodys env)
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
    )
  )
)

; Proc = ExpVal -> ExpVal

; Var * Exp * Env -> Proc
(define procedure
  (lambda (var body env)
    (lambda (val)
      (value-of body (extend-env var val env))
    )
  )
)

; Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (proc1 val)
  )
)

; Vars * Vars * Exps * Env -> Env
(define extend-env-rec
  (lambda (p-names b-vars p-bodys saved-env)
    (let ([vecs (map (lambda (x) (make-vector 1)) p-names)])
      (let ([new-env
          (let f ([ns p-names] [vcs vecs])
            (if (null? ns)
              saved-env
              (extend-env (car ns) (car vcs) (f (cdr ns) (cdr vcs)))
            )
          )
        ])
        (map
          (lambda (vec var body)
            (vector-set! vec 0
              (proc-val (procedure var body new-env))
            )
          )
          vecs b-vars p-bodys
        )
        new-env
      )
    )
  )
)
