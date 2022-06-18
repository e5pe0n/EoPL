#lang eopl

(require "data-structures.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (exp1)
        (cases answer (value-of exp1 (init-env) the-store)
          (an-answer (v1 new-store)
            v1
          )
        )
      )
    )
  )
)

; Exp x Env x Sto -> Answer
(define value-of
  (lambda (exp env store)
    (cases expression exp
      (const-exp (num)
        (an-answer (num-val num) store)
      )
      (var-exp (var)
        (an-answer
          (apply-env env var)
          store
        )
      )
      (diff-exp (exp1 exp2)
        (cases answer (value-of exp1 env store)
          (an-answer (v1 store1)
            (cases answer (value-of exp2 env store1)
              (an-answer (v2 store2)
                (an-answer
                  (num-val
                    (- (expval->num v1) (expval->num v2))
                  )
                  store2
                )
              )
            )
          )
        )
      )
      (zero?-exp (exp1)
        (cases answer (value-of exp1 env store)
          (an-answer (v1 new-store)
            (bool-val (zero? (expval->num v1)))
            new-store
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (cases answer (value-of exp1 env store)
          (an-answer (v1 new-store)
            (if (expval->bool v1)
              (value-of exp2 env new-store)
              (value-of exp3 env new-store)
            )
          )
        )
      )
      (let-exp (var exp1 body)
        (cases answer (value-of exp1 env store)
          (an-answer (v1 new-store)
            (value-of body (extend-env var v1 env) new-store)
          )
        )
      )
      (letrec-exp (p-name b-vars p-body letrec-body)
        (value-of
          letrec-body
          (extend-env-rec p-name b-vars p-body env)
          store
        )
      )
      (proc-exp (vars body)
        (an-answer
          (proc-val (procedure vars body env))
          store
        )
      )
      (call-exp (rator rands)
        (cases answer (value-of rator env store)
          (an-answer (v1 store1)
            (let f ([rs rands] [args '()] [sto store1])
              (if (null? rs)
                (apply-procedure (expval->proc v1) args sto)
                (cases answer (value-of (car rs) env sto)
                  (an-answer (v2 store2)
                    (f (cdr rs) (append args (list v2)) store2)
                  )
                )
              )
            )
          )
        )
      )
      (newref-exp (exp1)
        (cases answer (value-of exp1 env store)
          (an-answer (v1 new-store)
            (let ([res (newref v1)])
              (an-answer
                (ref-val (car res))
                (cdr res)
              )
            )
          )
        )
      )
      (deref-exp (exp1)
        (cases answer (value-of exp1 env store)
          (an-answer (v1 new-store)
            (an-answer (deref (expval->ref v1)) new-store)
          )
        )
      )
      (setref-exp (exp1 exp2)
        (cases answer (value-of exp1 env store)
          (an-answer (v1 store1)
            (cases answer (value-of exp2 env store1)
              (an-answer (v2 store2)
                (an-answer
                  (num-val 23)
                  (setref! (expval->ref v1) v2)
                )
              )
            )
          )
        )
      )
    )
  )
)

; Proc * ExpVal * Sto -> Answer
(define apply-procedure
  (lambda (proc1 vals store)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of body
          (let f ([vrs vars] [vls vals])
            (if (null? vrs)
              saved-env
              (extend-env (car vrs) (car vls) (f (cdr vrs) (cdr vls)))
            )
          )
          store
        )
      )
    )
  )
)