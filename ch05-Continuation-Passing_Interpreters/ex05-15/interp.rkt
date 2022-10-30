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
        (value-of/k exp1 (init-env) '())
      )
    )
  )
)

; FinalAnswer = ExpVal
; Cont = Listof(Frame)

(define-datatype frame frame?
  (zero1-frame)
  (let-exp-frame
    (vars (list-of identifier?))
    (exps (list-of expression?))
    (rvars (list-of identifier?))
    (vals (list-of expval?))
    (body expression?)
    (env environment?)
  )
  (if-test-frame
    (exp2 expression?)
    (exp3 expression?)
    (env environment?)
  )
  (binary-op1-frame
    (op procedure?)
    (exp2 expression?)
    (saved-env environment?)
  )
  (binary-op2-frame
    (op procedure?)
    (val1 expval?)
  )
  (rator-frame
    (rands (list-of expression?))
    (saved-env environment?)
  )
  (rand1-frame
    (rands (list-of expression?))
    (proc1 proc?)
    (saved-env environment?)
  )
  (rand2-frame
    (proc1 proc?)
  )
  (cons1-frame
    (exp2 expression?)
    (saved-env environment?)
  )
  (cons2-frame
    (val1 expval?)
    (saved-env environment?)
  )
  (null?-frame
    (saved-env environment?)
  )
  (car-frame
    (saved-env environment?)
  )
  (cdr-frame
    (saved-env environment?)
  )
  (list1-frame
    (exps (list-of expression?))
    (saved-env environment?)
  )
  (list2-frame
    (val1 expval?)
    (saved-env environment?)
  )
)

; Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (if (null? cont)
      ; end-cont
      (begin
        (eopl:printf "End of Computation.~%")
        val
      )
      (let ([saved-cont (cdr cont)])
        (cases frame (car cont)
          (zero1-frame ()
            (apply-cont saved-cont
              (bool-val (zero? (expval->num val)))
            )
          )
          (let-exp-frame (vars exps rvars vals body saved-env)
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
                (cons (let-exp-frame (cdr vars) (cdr exps) (cons (car vars) rvars) (cons val vals) body saved-env) saved-cont)
              )
            )
          )
          (if-test-frame (exp2 exp3 saved-env)
            (if (expval->bool val)
              (value-of/k exp2 saved-env saved-cont)
              (value-of/k exp3 saved-env saved-cont)
            )
          )
          (binary-op1-frame (op exp2 saved-env)
            (value-of/k exp2 saved-env
              (cons (binary-op2-frame op val) saved-cont)
            )
          )
          (binary-op2-frame (op val1)
            (let ([num1 (expval->num val1)] [num2 (expval->num val)])
              (apply-cont saved-cont
                (num-val (op num1 num2))
              )
            )
          )
          (rator-frame (rands saved-env)
            (let ([proc1 (expval->proc val)])
              (if (null? rands)
                (apply-procedure/k proc1 '() saved-cont)
                (value-of/k (car rands) saved-env
                  (let ([rds (cdr rands)])
                    (if (null? rds)
                      (cons (rand2-frame proc1) saved-cont)
                      (cons (rand1-frame rds proc1 saved-env) saved-cont)
                    )
                  )
                )
              )
            )
          )
          (rand1-frame (rands proc1 saved-env)
            (value-of/k (car rands) saved-env
              (cases proc proc1
                (procedure (vars body p-saved-env)
                  (let (
                      [rds (cdr rands)]
                      [proc2 (procedure (cdr vars) body (extend-env (car vars) val p-saved-env))]
                    )
                    (if (null? rds)
                      (cons (rand2-frame proc2) saved-cont)
                      (cons (rand1-frame rds proc2 saved-env) saved-cont)
                    )
                  )
                )
              )
            )
          )
          (rand2-frame (proc1)
            (apply-procedure/k proc1 (list val) saved-cont)
          )
          (cons1-frame (exp2 saved-env)
            (value-of/k exp2 saved-env
              (cons (cons2-frame val saved-env) saved-cont)
            )
          )
          (cons2-frame (val1 saved-env)
            (let ([any1 (expval->any val1)] [any2 (expval->any val)])
              (apply-cont saved-cont
                (list-val (cons any1 any2))
              )
            )
          )
          (null?-frame (saved-env)
            (apply-cont saved-cont
              (bool-val (null? (expval->list val)))
            )
          )
          (car-frame (saved-env)
            (let ([list1 (expval->list val)])
              (apply-cont saved-cont
                (any->expval (car list1))
              )
            )
          )
          (cdr-frame (saved-env)
            (let ([list1 (expval->list val)])
              (apply-cont saved-cont
                (any->expval (cdr list1))
              )
            )
          )
          (list1-frame (exps saved-env)
            (value-of/k (list-exp exps) saved-env
              (list2-frame val saved-env saved-cont)
            )
          )
          (list2-frame (val1 saved-env)
            (apply-cont saved-cont (list-val (cons (expval->any val1) (expval->list val)))
            )
          )
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
        (value-of/k exp1 env (cons (zero1-frame) cont))
      )
      (let-exp (vars exps body)
        (if (null? vars)
          (value-of/k body env cont)
          (value-of/k (car exps) env
            (cons (let-exp-frame (cdr vars) (cdr exps) (list (car vars)) '() body env) cont)
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (cons (if-test-frame exp2 exp3 env) cont)
        )
      )
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (cons (binary-op1-frame - exp2 env) cont)
        )
      )
      (mul-exp (exp1 exp2)
        (value-of/k exp1 env
          (cons (binary-op1-frame * exp2 env) cont)
        )
      )
      (call-exp (rator rands)
        (value-of/k rator env
          (cons (rator-frame rands env) cont)
        )
      )
      (emptylist-exp () (apply-cont cont (list-val '())))
      (cons-exp (exp1 exp2)
        (value-of/k exp1 env
          (cons (cons1-frame exp2 env) cont)
        )
      )
      (null?-exp (exp1)
        (value-of/k exp1 env
          (cons (null?-frame env) cont)
        )
      )
      (car-exp (exp1)
        (value-of/k exp1 env
          (cons (car-frame env) cont)
        )
      )
      (cdr-exp (exp1)
        (value-of/k exp1 env
          (cons (cdr-frame env) cont)
        )
      )
      (list-exp (exps)
        (if (null? exps)
          (apply-cont cont (list-val '()))
          (value-of/k (car exps) env
            (cons (list1-frame (cdr exps) env) cont)
          )
        )
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
    )
  )
)
