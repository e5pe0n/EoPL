#lang eopl

(require "utils.rkt")
(require "data-structures.rkt")
(require "lang.rkt")

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
    (rand expression?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (rand-cont
    (val1 expval?)
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
      (zero1-cont (saved-cont)
        (apply-cont saved-cont
          (bool-val (zero? (expval->num val)))
        )
      )
      (let-exp-cont (vars exps rvars vals body saved-env saved-cont)
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
      (if-test-cont (exp2 exp3 saved-env saved-cont)
        (if (expval->bool val)
          (value-of/k exp2 saved-env saved-cont)
          (value-of/k exp3 saved-env saved-cont)
        )
      )
      (binary-op1-cont (op exp2 saved-env saved-cont)
        (value-of/k exp2 saved-env
          (binary-op2-cont op val saved-cont)
        )
      )
      (binary-op2-cont (op val1 saved-cont)
        (let ([num1 (expval->num val1)] [num2 (expval->num val)])
          (apply-cont saved-cont
            (num-val (op num1 num2))
          )
        )
      )
      (rator-cont (rand saved-env saved-cont)
        (value-of/k rand saved-env
          (rand-cont val saved-cont)
        )
      )
      (rand-cont (val1 saved-cont)
        (let ([proc1 (expval->proc val1)])
          (apply-procedure/k proc1 val saved-cont)
        )
      )
      (cons1-cont (exp2 saved-env saved-cont)
        (value-of/k exp2 saved-env
          (cons2-cont val saved-env saved-cont)
        )
      )
      (cons2-cont (val1 saved-env saved-cont)
        (let ([any1 (expval->any val1)] [any2 (expval->any val)])
          (apply-cont saved-cont
            (list-val (cons any1 any2))
          )
        )
      )
      (null?-cont (saved-env saved-cont)
        (apply-cont saved-cont
          (bool-val (null? (expval->list val)))
        )
      )
      (car-cont (saved-env saved-cont)
        (let ([list1 (expval->list val)])
          (apply-cont saved-cont
            (any->expval (car list1))
          )
        )
      )
      (cdr-cont (saved-env saved-cont)
        (let ([list1 (expval->list val)])
          (apply-cont saved-cont
            (any->expval (cdr list1))
          )
        )
      )
      (list1-cont (exps saved-env saved-cont)
        (value-of/k (list-exp exps) saved-env
          (list2-cont val saved-env saved-cont)
        )
      )
      (list2-cont (val1 saved-env saved-cont)
        (apply-cont saved-cont (list-val (cons (expval->any val1) (expval->list val)))
        )
      )
    )
  )
)

; Exp x Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (proc-exp (var body)
        (apply-cont cont
          (proc-val (procedure var body env))
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
      (call-exp (rator rand)
        (value-of/k rator env
          (rator-cont rand env cont)
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


; Proc * ExpVal * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 val cont)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of/k body (extend-env var val saved-env) cont)
      )
    )
  )
)
