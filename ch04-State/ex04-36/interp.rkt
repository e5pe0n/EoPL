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

; Exp * Env -> Ref
(define value-of-operand
  (lambda (exp env)
    (cases expression exp
      (var-exp (var) (apply-env env var))
      (else
        (newref (value-of exp env))
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
      (letref-exp (var exp1 body)
        (value-of body
          (extend-env var (value-of-operand exp1 env) env)
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
      (procv-exp (var body)
        (procv-val (procedure var body env))
      )
      (call-exp (rator rand)
        (let ([proc1 (value-of rator env)])
          (cases expval proc1
            (proc-val (proc1)
              (let ([arg (value-of-operand rand env)])
                (apply-procedure proc1 arg)
              )
            )
            (procv-val (proc1)
              (let ([arg (value-of rand env)])
                (apply-procedure proc1 (newref arg))
              )
            )
            (else (eopl:error 'invalid-proc' "proc1 must be proc-val; received proc1=~s" proc1))
          )
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
      (newarray-exp (exps)
        (array-val
          (make-array (map (lambda (x) (value-of x env)) exps))
        )
      )
      (arrayref-exp (exp1 exp2)
        (let ([array (expval->array (value-of exp1 env))] [n (expval->num (value-of exp2 env))])
          (ref-val
            (arrayref array n)
          )
        )
      )
      (print-exp (exp1)
        (print (value-of exp1 env))
        (num-val 999)
      )
    )
  )
)

; Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body (extend-env var val saved-env))
      )
    )
  )
)

