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
        (value-of/k exp1 (init-env) (end-cont))
      )
    )
  )
)

; FinalAnswer = ExpVal

(define end-cont
  (lambda ()
    (cons
      (lambda (val)
        (begin
          (eopl:printf "End of Computation.~%")
          val
        )
      )
      (lambda (val)
        (lambda ()
          (report-uncaught-exception)
        )
      )
    )
  )
)
(define zero1-cont
  (lambda (saved-cont)
    (cons
      (lambda (val)
        (apply-cont saved-cont
          (bool-val (zero? (expval->num val)))
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define let-exp-cont
  (lambda (vars exps rvars vals body saved-env saved-cont)
    (cons
      (lambda (val)
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
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define if-test-cont
  (lambda (exp2 exp3 saved-env saved-cont)
    (cons
      (lambda (val)
        (if (expval->bool val)
          (value-of/k exp2 saved-env saved-cont)
          (value-of/k exp3 saved-env saved-cont)
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define binary-op1-cont
  (lambda (op exp2 saved-env saved-cont)
    (cons
      (lambda (val)
        (value-of/k exp2 saved-env
          (binary-op2-cont op val saved-cont)
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define binary-op2-cont
  (lambda (op val1 saved-cont)
    (cons
      (lambda (val)
        (let ([num1 (expval->num val1)] [num2 (expval->num val)])
          (apply-cont saved-cont
            (num-val (op num1 num2))
          )
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define div1-cont
  (lambda (exp2 saved-env saved-cont)
    (cons
      (lambda (val)
        (value-of/k exp2 saved-env
          (div2-cont val saved-cont)
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define div2-cont
  (lambda (val1 saved-cont)
    (cons
      (lambda (val)
        (let ([num1 (expval->num val1)] [num2 (expval->num val)])
          (if (zero? num2)
            (apply-cont (raise1-cont saved-cont) (num-val 101))
            (apply-cont saved-cont (num-val (/ num1 num2)))
          )
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define rator-cont
  (lambda (rands saved-env saved-cont)
    (cons
      (lambda (val)
        (let ([proc1 (expval->proc val)])
          (cases proc proc1
            (procedure (vars body p-saved-env)
              (if (not (= (length vars) (length rands)))
                (apply-cont (raise1-cont saved-cont) (num-val 100))
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
            (procedure-cont (p-saved-cont)
              (value-of/k (car rands) saved-env (rand2-cont proc1 p-saved-cont))
            )
          )
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define rand1-cont
  (lambda (rands proc1 saved-env saved-cont)
    (cons
      (lambda (val)
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
            (else report-invalid-case)
          )
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define rand2-cont
  (lambda (proc1 saved-cont)
    (cons
      (lambda (val)
        (apply-procedure/k proc1 (list val) saved-cont)
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define cons1-cont
  (lambda (exp2 saved-env saved-cont)
    (cons
      (lambda (val)
        (value-of/k exp2 saved-env
          (cons2-cont val saved-env saved-cont)
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define cons2-cont
  (lambda (val1 saved-env saved-cont)
    (cons
      (lambda (val)
        (let ([any1 (expval->any val1)] [any2 (expval->any val)])
          (apply-cont saved-cont
            (list-val (cons any1 any2))
          )
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define null?-cont
  (lambda (saved-env saved-cont)
    (cons
      (lambda (val)
        (apply-cont saved-cont
          (bool-val (null? (expval->list val)))
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define car-cont
  (lambda (saved-env saved-cont)
    (cons
      (lambda (val)
        (let ([list1 (expval->list val)])
          (apply-cont saved-cont
            (any->expval (car list1))
          )
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define cdr-cont
  (lambda (saved-env saved-cont)
    (cons
      (lambda (val)
        (let ([list1 (expval->list val)])
          (apply-cont saved-cont
            (any->expval (cdr list1))
          )
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define list1-cont
  (lambda (exps saved-env saved-cont)
    (cons
      (lambda (val)
        (value-of/k (list-exp exps) saved-env
          (list2-cont val saved-env saved-cont)
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define list2-cont
  (lambda (val1 saved-env saved-cont)
    (cons
      (lambda (val)
        (apply-cont
          saved-cont
          (list-val (cons (expval->any val1) (expval->list val)))
        )
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)
(define try-cont
  (lambda (var handler-exp saved-env saved-cont)
    (cons
      (lambda (val)
        (apply-cont saved-cont val)
      )
      (lambda (val)
        (value-of/k handler-exp
          (extend-env var val saved-env)
          saved-cont
        )
      )
    )
  )
)
(define raise1-cont
  (lambda (saved-cont)
    (cons
      (lambda (val)
        (apply-handler val saved-cont)
      )
      (lambda (val)
        (apply-handler val saved-cont)
      )
    )
  )
)

; Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    ((car cont) val)
  )
)

; ExpVal * Cont -> FinalAnswer
(define apply-handler
  (lambda (val cont)
    ((cdr cont) val)
  )
)

; Exp x Env * Cont -> FinalAnswer
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
      (letrec-exp (p-name b-vars p-body letrec-body)
        (value-of/k
          letrec-body
          (extend-env-rec p-name b-vars p-body env)
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
      (div-exp (exp1 exp2)
        (value-of/k exp1 env
          (div1-cont exp2 env cont)
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
      (try-exp (exp1 var handler-exp)
        (value-of/k exp1 env (try-cont var handler-exp env cont))
      )
      (raise-exp (exp1)
        (value-of/k exp1 env (raise1-cont cont))
      )
      (letcc-exp (var body)
        (value-of/k body (extend-env var (proc-val (procedure-cont cont)) env) cont)
      )
    )
  )
)

; Proc * Listof(ExpVal) * Cont -> FinalAnswer
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
      (procedure-cont (saved-cont)
        (apply-cont saved-cont (car vals))
      )
    )
  )
)
