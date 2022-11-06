#lang eopl

(require "utils.rkt")
(require "data-structures.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; FinalAnswer = ExpVal
; Bounce = ExpVal | (() -> Bounce)

; Program -> FinalAnswer
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (trampoline
          (value-of/k exp1 (init-env) (end-cont))
        )
      )
    )
  )
)

(define-datatype bounce bounce?
  (zero1-bounce
    (cont continuation?)
    (val expval?)
  )
  (let-exp-bounce
    (vars (list-of identifier?))
    (exps (list-of expression?))
    (rvars (list-of identifier?))
    (vals (list-of expval?))
    (body expression?)
    (env environment?)
    (cont continuation?)
    (val expval?)
  )
  (if-test-bounce
    (exp2 expression?)
    (exp3 expression?)
    (env environment?)
    (cont continuation?)
    (val expval?)
  )
  (binary-op1-bounce
    (op procedure?)
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?)
    (val expval?)
  )
  (binary-op2-bounce
    (op procedure?)
    (val1 expval?)
    (saved-cont continuation?)
    (val expval?)
  )
  (rator-bounce
    (rands (list-of expression?))
    (saved-env environment?)
    (saved-cont continuation?)
    (val expval?)
  )
  (rand1-bounce
    (rands (list-of expression?))
    (proc1 proc?)
    (saved-env environment?)
    (saved-cont continuation?)
    (val expval?)
  )
  (rand2-bounce
    (proc1 proc?)
    (saved-cont continuation?)
    (val expval?)
  )
  (cons1-bounce
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?)
    (val expval?)
  )
  (cons2-bounce
    (val1 expval?)
    (saved-env environment?)
    (saved-cont continuation?)
    (val expval?)
  )
  (null?-bounce
    (saved-env environment?)
    (saved-cont continuation?)
    (val expval?)
  )
  (car-bounce
    (saved-env environment?)
    (saved-cont continuation?)
    (val expval?)
  )
  (cdr-bounce
    (saved-env environment?)
    (saved-cont continuation?)
    (val expval?)
  )
  (list1-bounce
    (exps (list-of expression?))
    (saved-env environment?)
    (saved-cont continuation?)
    (val expval?)
  )
  (list2-bounce
    (val1 expval?)
    (saved-env environment?)
    (saved-cont continuation?)
    (val expval?)
  )
)

; Bounce -> FinalAnswer
(define trampoline
  (lambda (bnc)
    (if (expval? bnc)
      bnc
      (trampoline
        (cases bounce bnc
          (zero1-bounce (saved-cont val)
            (apply-cont saved-cont
              (bool-val (zero? (expval->num val)))
            )
          )
          (let-exp-bounce (vars exps rvars vals body saved-env saved-cont val)
            (if (null? exps)
              (value-of/k body
                (let f ([vrs rvars] [vls (cons val vals)])
                  (if (null? vrs)
                    saved-env
                    (extend-env (car vrs) (car vls) (f (cdr vrs) (cdr vls)))
                  )
                )
                saved-cont
              )
              (value-of/k (car exps)
                saved-env
                (let-exp-cont (cdr vars) (cdr exps) (cons (car vars) rvars) (cons val vals) body saved-env saved-cont)
              )
            )
          )
          (if-test-bounce (exp2 exp3 saved-env saved-cont val)
            (if (expval->bool val)
              (value-of/k exp2 saved-env saved-cont)
              (value-of/k exp3 saved-env saved-cont)
            )
          )
          (binary-op1-bounce (op exp2 saved-env saved-cont val)
            (value-of/k exp2 saved-env
              (binary-op2-cont op val saved-cont)
            )
          )
          (binary-op2-bounce (op val1 saved-cont val)
            (let ([num1 (expval->num val1)] [num2 (expval->num val)])
              (apply-cont saved-cont
                (num-val (op num1 num2))
              )
            )
          )
          (rator-bounce (rands saved-env saved-cont val)
            (let ([proc1 (expval->proc val)])
              (if (null? rands)
                (apply-procedure/k proc1 '() saved-cont)
                (value-of/k (car rands) saved-env
                  (let ([rds (cdr rands)])
                    (if (null? rds)
                      (rand2-cont proc1 saved-cont)
                      (rand1-cont rds proc1 saved-env saved-cont)
                    )
                  )
                )
              )
            )
          )
          (rand1-bounce (rands proc1 saved-env saved-cont val)
            (value-of/k (car rands) saved-env
              (cases proc proc1
                (procedure (vars body p-saved-env)
                  (let (
                      [rds (cdr rands)]
                      [proc2 (procedure (cdr vars) body (extend-env (car vars) val p-saved-env))]
                    )
                    (if (null? rds)
                      (rand2-cont proc2 saved-cont)
                      (rand1-cont rds proc2 saved-env saved-cont)
                    )
                  )
                )
              )
            )
          )
          (rand2-bounce (proc1 saved-cont val)
            (apply-procedure/k proc1 (list val) saved-cont)
          )
          (cons1-bounce (exp2 saved-env saved-cont val)
            (value-of/k exp2 saved-env
              (cons2-cont val saved-env saved-cont)
            )
          )
          (cons2-bounce (val1 saved-env saved-cont val)
            (let ([any1 (expval->any val1)] [any2 (expval->any val)])
              (apply-cont saved-cont
                (list-val (cons any1 any2))
              )
            )
          )
          (null?-bounce (saved-env saved-cont val)
            (apply-cont saved-cont
              (bool-val (null? (expval->list val)))
            )
          )
          (car-bounce (saved-env saved-cont val)
            (let ([list1 (expval->list val)])
              (apply-cont saved-cont
                (any->expval (car list1))
              )
            )
          )
          (cdr-bounce (saved-env saved-cont val)
            (let ([list1 (expval->list val)])
              (apply-cont saved-cont
                (any->expval (cdr list1))
              )
            )
          )
          (list1-bounce (exps saved-env saved-cont val)
            (value-of/k (list-exp exps) saved-env
              (list2-cont val saved-env saved-cont)
            )
          )
          (list2-bounce (val1 saved-env saved-cont val)
            (apply-cont saved-cont (list-val (cons (expval->any val1) (expval->list val)))
            )
          )
        )
      )
    )
  )
)

(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont (cont continuation?))
  (let-exp-cont
    (vars (list-of identifier?))
    (exps (list-of expression?))
    (rvars (list-of identifier?))
    (vals (list-of expval?))
    (body expression?)
    (env environment?)
    (cont continuation?)
  )
  (if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (env environment?)
    (cont continuation?)
  )
  (binary-op1-cont
    (op procedure?)
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (binary-op2-cont
    (op procedure?)
    (val1 expval?)
    (saved-cont continuation?)
  )
  (rator-cont
    (rands (list-of expression?))
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (rand1-cont
    (rands (list-of expression?))
    (proc1 proc?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (rand2-cont
    (proc1 proc?)
    (saved-cont continuation?)
  )
  (cons1-cont
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (cons2-cont
    (val1 expval?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (null?-cont
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (car-cont
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (cdr-cont
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (list1-cont
    (exps (list-of expression?))
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (list2-cont
    (val1 expval?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
)

; Cont * ExpVal -> Bounce
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
        (begin
          (eopl:printf "End of Computation.~%")
          val
        )
      )
      (zero1-cont (saved-cont)
        (zero1-bounce saved-cont val)
      )
      (let-exp-cont (vars exps rvars vals body saved-env saved-cont)
        (let-exp-bounce vars exps rvars vals body saved-env saved-cont val)
      )
      (if-test-cont (exp2 exp3 saved-env saved-cont)
        (if-test-bounce exp2 exp3 saved-env saved-cont val)
      )
      (binary-op1-cont (op exp2 saved-env saved-cont)
        (binary-op1-bounce op exp2 saved-env saved-cont val)
      )
      (binary-op2-cont (op val1 saved-cont)
        (binary-op2-bounce op val1 saved-cont val)
      )
      (rator-cont (rands saved-env saved-cont)
        (rator-bounce rands saved-env saved-cont val)
      )
      (rand1-cont (rands proc1 saved-env saved-cont)
        (rand1-bounce rands proc1 saved-env saved-cont val)
      )
      (rand2-cont (proc1 saved-cont)
        (rand2-bounce proc1 saved-cont val)
      )
      (cons1-cont (exp2 saved-env saved-cont)
        (cons1-bounce exp2 saved-env saved-cont val)
      )
      (cons2-cont (val1 saved-env saved-cont)
        (cons2-bounce val1 saved-env saved-cont val)
      )
      (null?-cont (saved-env saved-cont)
        (null?-bounce saved-env saved-cont val)
      )
      (car-cont (saved-env saved-cont)
        (car-bounce saved-env saved-cont val)
      )
      (cdr-cont (saved-env saved-cont)
        (cdr-bounce (saved-env saved-cont val))
      )
      (list1-cont (exps saved-env saved-cont)
        (list1-bounce (exps saved-env saved-cont val))
      )
      (list2-cont (val1 saved-env saved-cont)
        (list2-bounce (val1 saved-env saved-cont val))
      )
    )
  )
)

; Exp x Env * Cont -> Bounce
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (proc-exp (vars body)
        (apply-cont cont
          (proc-val (procedure vars body env))
        )
      )
      (letrec-exp (p-name b-var p-body letrec-body)
        (value-of/k
          letrec-body
          (extend-env-rec p-name b-var p-body env)
          cont
        )
      )
      (zero?-exp (exp1)
        (value-of/k exp1 env (zero1-cont cont))
      )
      (let-exp (vars exps body)
        (if (null? vars)
          (value-of/k body env cont)
          (value-of/k (car exps) env
            (let-exp-cont (cdr vars) (cdr exps) (list (car vars)) '() body env cont)
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (if-test-cont exp2 exp3 env cont)
        )
      )
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (binary-op1-cont - exp2 env cont)
        )
      )
      (mul-exp (exp1 exp2)
        (value-of/k exp1 env
          (binary-op1-cont * exp2 env cont)
        )
      )
      (call-exp (rator rands)
        (value-of/k rator env
          (rator-cont rands env cont)
        )
      )
      (emptylist-exp () (apply-cont cont (list-val '())))
      (cons-exp (exp1 exp2)
        (value-of/k exp1 env
          (cons1-cont exp2 env cont)
        )
      )
      (null?-exp (exp1)
        (value-of/k exp1 env
          (null?-cont env cont)
        )
      )
      (car-exp (exp1)
        (value-of/k exp1 env
          (car-cont env cont)
        )
      )
      (cdr-exp (exp1)
        (value-of/k exp1 env
          (cdr-cont env cont)
        )
      )
      (list-exp (exps)
        (if (null? exps)
          (apply-cont cont (list-val '()))
          (value-of/k (car exps) env
            (list1-cont (cdr exps) env cont)
          )
        )
      )
    )
  )
)


; Proc * Listof(ExpVal) * Cont -> ExpVal
(define apply-procedure/k
  (lambda (proc1 vals cont)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of/k body
          (if (null? vars)
            saved-env
            (extend-env (car vars) (car vals) saved-env)
          )
          cont
        )
      )
    )
  )
)
