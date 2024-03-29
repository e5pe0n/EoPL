#lang eopl

(require "utils.rkt")
(require "data-structures.rkt")
(require "lang.rkt")
(require "errors.rkt")

(provide (all-defined-out))

; Program -> FinalAnswer
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of/k exp1 (init-env) (end-cont) (end-cont))
      )
    )
  )
)

; FinalAnswer = ExpVal

(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont
    (cont continuation?)
    (exc-cont continuation?)
  )
  (let-exp-cont
    (vars (list-of identifier?))
    (exps (list-of expression?))
    (rvars (list-of identifier?))
    (vals (list-of expval?))
    (body expression?)
    (env environment?)
    (cont continuation?)
    (exc-cont continuation?)
  )
  (if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (env environment?)
    (cont continuation?)
    (exc-cont continuation?)
  )
  (binary-op1-cont
    (op procedure?)
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?)
    (saved-exc-cont continuation?)
  )
  (binary-op2-cont
    (op procedure?)
    (val1 expval?)
    (saved-cont continuation?)
    (saved-exc-cont continuation?)
  )
  (rator-cont
    (rands (list-of expression?))
    (saved-env environment?)
    (saved-cont continuation?)
    (saved-exc-cont continuation?)
  )
  (rand1-cont
    (rands (list-of expression?))
    (proc1 proc?)
    (saved-env environment?)
    (saved-cont continuation?)
    (saved-exc-cont continuation?)
  )
  (rand2-cont
    (proc1 proc?)
    (saved-cont continuation?)
    (saved-exc-cont continuation?)
  )
  (cons1-cont
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?)
    (saved-exc-cont continuation?)
  )
  (cons2-cont
    (val1 expval?)
    (saved-env environment?)
    (saved-cont continuation?)
    (saved-exc-cont continuation?)
  )
  (null?-cont
    (saved-env environment?)
    (saved-cont continuation?)
    (saved-exc-cont continuation?)
  )
  (car-cont
    (saved-env environment?)
    (saved-cont continuation?)
    (saved-exc-cont continuation?)
  )
  (cdr-cont
    (saved-env environment?)
    (saved-cont continuation?)
    (saved-exc-cont continuation?)
  )
  (list1-cont
    (exps (list-of expression?))
    (saved-env environment?)
    (saved-cont continuation?)
    (saved-exc-cont continuation?)
  )
  (list2-cont
    (val1 expval?)
    (saved-env environment?)
    (saved-cont continuation?)
    (saved-exc-cont continuation?)
  )
  (try-cont
    (var identifier?)
    (handler-exp expression?)
    (env environment?)
    (cont continuation?)
    (saved-exc-cont continuation?)
  )
  (raise1-cont
    (saved-exc-cont continuation?)
  )
)

; Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
        (begin
          (eopl:printf "End of Computation.~%")
          val
        )
      )
      (zero1-cont (saved-cont saved-exc-cont)
        (apply-cont saved-cont
          (bool-val (zero? (expval->num val)))
        )
      )
      (let-exp-cont (vars exps rvars vals body saved-env saved-cont saved-exc-cont)
        (if (null? exps)
          (value-of/k body
            (let f ([vrs rvars] [vls (cons val vals)])
              (if (null? vrs)
                saved-env
                (extend-env (car vrs) (car vls) (f (cdr vrs) (cdr vls)))
              )
            )
            saved-cont
            saved-exc-cont
          )
          (value-of/k (car exps)
            saved-env
            (let-exp-cont (cdr vars) (cdr exps) (cons (car vars) rvars) (cons val vals) body saved-env saved-cont saved-exc-cont)
            saved-exc-cont
          )
        )
      )
      (if-test-cont (exp2 exp3 saved-env saved-cont saved-exc-cont)
        (if (expval->bool val)
          (value-of/k exp2 saved-env saved-cont saved-exc-cont)
          (value-of/k exp3 saved-env saved-cont saved-exc-cont)
        )
      )
      (binary-op1-cont (op exp2 saved-env saved-cont saved-exc-cont)
        (value-of/k exp2 saved-env
          (binary-op2-cont op val saved-cont saved-exc-cont)
          saved-exc-cont
        )
      )
      (binary-op2-cont (op val1 saved-cont saved-exc-cont)
        (let ([num1 (expval->num val1)] [num2 (expval->num val)])
          (apply-cont saved-cont
            (num-val (op num1 num2))
          )
        )
      )
      (rator-cont (rands saved-env saved-cont saved-exc-cont)
        (let ([proc1 (expval->proc val)])
          (if (null? rands)
            (apply-procedure/k proc1 '() saved-cont)
            (value-of/k (car rands) saved-env
              (let ([rds (cdr rands)])
                (if (null? rds)
                  (rand2-cont proc1 saved-cont saved-exc-cont)
                  (rand1-cont rds proc1 saved-env saved-cont saved-exc-cont)
                )
              )
              saved-exc-cont
            )
          )
        )
      )
      (rand1-cont (rands proc1 saved-env saved-cont saved-exc-cont)
        (value-of/k (car rands) saved-env
          (cases proc proc1
            (procedure (vars body p-saved-env)
              (let (
                  [rds (cdr rands)]
                  [proc2 (procedure (cdr vars) body (extend-env (car vars) val p-saved-env))]
                )
                (if (null? rds)
                  (rand2-cont proc2 saved-cont saved-exc-cont)
                  (rand1-cont rds proc2 saved-env saved-cont saved-exc-cont)
                )
              )
            )
          )
          saved-exc-cont
        )
      )
      (rand2-cont (proc1 saved-cont saved-exc-cont)
        (apply-procedure/k proc1 (list val) saved-cont saved-exc-cont)
      )
      (cons1-cont (exp2 saved-env saved-cont saved-exc-cont)
        (value-of/k exp2 saved-env
          (cons2-cont val saved-env saved-cont saved-exc-cont)
          saved-exc-cont
        )
      )
      (cons2-cont (val1 saved-env saved-cont saved-exc-cont)
        (let ([any1 (expval->any val1)] [any2 (expval->any val)])
          (apply-cont saved-cont
            (list-val (cons any1 any2))
          )
        )
      )
      (null?-cont (saved-env saved-cont saved-exc-cont)
        (apply-cont saved-cont
          (bool-val (null? (expval->list val)))
        )
      )
      (car-cont (saved-env saved-cont saved-exc-cont)
        (let ([list1 (expval->list val)])
          (apply-cont saved-cont
            (any->expval (car list1))
          )
        )
      )
      (cdr-cont (saved-env saved-cont saved-exc-cont)
        (let ([list1 (expval->list val)])
          (apply-cont saved-cont
            (any->expval (cdr list1))
          )
        )
      )
      (list1-cont (exps saved-env saved-cont saved-exc-cont)
        (value-of/k (list-exp exps) saved-env
          (list2-cont val saved-env saved-cont saved-exc-cont)
          saved-exc-cont
        )
      )
      (list2-cont (val1 saved-env saved-cont saved-exc-cont)
        (apply-cont saved-cont (list-val (cons (expval->any val1) (expval->list val)))
        )
      )
      (try-cont (var handler-exp saved-env saved-cont saved-exc-cont)
        (apply-cont saved-cont val)
      )
      (raise1-cont (saved-exc-cont)
        (apply-handler val saved-exc-cont)
      )
    )
  )
)


; Exp * Env * Cont * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont exc-cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (proc-exp (vars body)
        (apply-cont cont
          (proc-val (procedure vars body env))
        )
      )
      (letrec-exp (p-name b-vars p-body letrec-body)
        (value-of/k
          letrec-body
          (extend-env-rec p-name b-vars p-body env)
          cont
          exc-cont
        )
      )
      (zero?-exp (exp1)
        (value-of/k exp1 env (zero1-cont cont exc-cont) exc-cont)
      )
      (let-exp (vars exps body)
        (if (null? vars)
          (value-of/k body env cont exc-cont)
          (value-of/k (car exps) env
            (let-exp-cont (cdr vars) (cdr exps) (list (car vars)) '() body env cont exc-cont)
            exc-cont
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (if-test-cont exp2 exp3 env cont exc-cont)
          exc-cont
        )
      )
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (binary-op1-cont - exp2 env cont exc-cont)
          exc-cont
        )
      )
      (mul-exp (exp1 exp2)
        (value-of/k exp1 env
          (binary-op1-cont * exp2 env cont exc-cont)
          exc-cont
        )
      )
      (call-exp (rator rands)
        (value-of/k rator env
          (rator-cont rands env cont exc-cont)
          exc-cont
        )
      )
      (emptylist-exp () (apply-cont cont (list-val '())))
      (cons-exp (exp1 exp2)
        (value-of/k exp1 env
          (cons1-cont exp2 env cont exc-cont)
          exc-cont
        )
      )
      (null?-exp (exp1)
        (value-of/k exp1 env
          (null?-cont env cont exc-cont)
          exc-cont
        )
      )
      (car-exp (exp1)
        (value-of/k exp1 env
          (car-cont env cont exc-cont)
          exc-cont
        )
      )
      (cdr-exp (exp1)
        (value-of/k exp1 env
          (cdr-cont env cont exc-cont)
          exc-cont
        )
      )
      (list-exp (exps)
        (if (null? exps)
          (apply-cont cont (list-val '()))
          (value-of/k (car exps) env
            (list1-cont (cdr exps) env cont exc-cont)
            exc-cont
          )
        )
      )
      (try-exp (exp1 var handler-exp)
        (value-of/k exp1 env
          (try-cont var handler-exp env cont exc-cont)
          (try-cont var handler-exp env cont exc-cont)
        )
      )
      (raise-exp (exp1)
        (value-of/k exp1 env (raise1-cont exc-cont) exc-cont)
      )
    )
  )
)


; Proc * Listof(ExpVal) * Cont * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 vals cont exc-cont)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of/k body
          (if (null? vars)
            saved-env
            (extend-env (car vars) (car vals) saved-env)
          )
          cont
          exc-cont
        )
      )
    )
  )
)


; ExpVal * Cont -> FinalAnswer
(define apply-handler
  (lambda (val cont)
    (cases continuation cont
      (try-cont (var handler-exp saved-env saved-cont saved-exc-cont)
        (value-of/k handler-exp
          (extend-env var val saved-env)
          saved-cont
          saved-exc-cont
        )
      )
      (else report-uncaught-exception)
    )
  )
)
