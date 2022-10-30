#lang eopl

(require "utils.rkt")
(require "data-structures.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; Program -> FinalAnswer
(define result-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (stmt1)
        (result-of stmt1 (init-env) (end-cmd-cont))
      )
    )
  )
)

(define-datatype continuation continuation?
  (assign-cont
    (var identifier?)
    (saved-env environment?)
    (saved-cmd-cont procedure?)
  )
  (print-cont
    (saved-cmd-cont procedure?)
  )
  (if-test-cont
    (stmt1 statement?)
    (stmt2 statement?)
    (saved-env environment?)
    (saved-cmd-cont procedure?)
  )
  (while-test-cont
    (exp1 expression?)
    (stmt1 statement?)
    (saved-env environment?)
    (saved-cmd-cont procedure?)
  )

  (zero1-cont (cont continuation?))
  (not-cont (cont continuation?))
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
)

; CmdCont -> ()
(define apply-cmd-cont
  (lambda (cmd-cont)
    (cmd-cont)
  )
)

(define end-cmd-cont
  (lambda ()
    (lambda ()
        (eopl:printf "End of Computation.~%")
    )
  )
)
(define assign-cmd-cont
  (lambda (var val saved-env saved-cmd-cont)
    (lambda ()
      (begin
        (setref! (apply-env saved-env var) val)
        (apply-cmd-cont saved-cmd-cont)
      )
    )
  )
)
(define print-cmd-cont
  (lambda (val saved-cmd-cont)
    (lambda ()
      (begin
        (println val)
        (apply-cmd-cont saved-cmd-cont)
      )
    )
  )
)
(define begin-cmd-cont
  (lambda (stmts saved-env saved-cmd-cont)
    (lambda ()
      (if (null? stmts)
        (apply-cmd-cont saved-cmd-cont)
        (result-of (car stmts) saved-env (begin-cmd-cont (cdr stmts) saved-env saved-cmd-cont))
      )
    )
  )
)
(define if-cmd-cont
  (lambda (val stmt1 stmt2 saved-env saved-cmd-cont)
    (lambda ()
      (if (expval->bool val)
        (result-of stmt1 saved-env saved-cmd-cont)
        (result-of stmt2 saved-env saved-cmd-cont)
      )
    )
  )
)
(define while-cmd-cont
  (lambda (val exp1 stmt1 saved-env saved-cmd-cont)
    (lambda ()
      (if (expval->bool val)
        (result-of (begin-stmt (list stmt1 (while-stmt exp1 stmt1))) saved-env saved-cmd-cont
        )
        (apply-cmd-cont saved-cmd-cont)
      )
    )
  )
)

; Cont * ExpVal -> ExpVal
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (assign-cont (var saved-env saved-cmd-cont)
        (apply-cmd-cont (assign-cmd-cont var val saved-env saved-cmd-cont))
      )
      (print-cont (saved-cmd-cont)
        (apply-cmd-cont (print-cmd-cont val saved-cmd-cont))
      )
      (if-test-cont (stmt1 stmt2 saved-env saved-cmd-cont)
        (apply-cmd-cont (if-cmd-cont val stmt1 stmt2 saved-env saved-cmd-cont))
      )
      (while-test-cont (exp1 stmt1 saved-env saved-cmd-cont)
        (apply-cmd-cont (while-cmd-cont val exp1 stmt1 saved-env saved-cmd-cont))
      )

      (zero1-cont (saved-cont)
        (apply-cont saved-cont
          (bool-val (zero? (expval->num val)))
        )
      )
      (not-cont (saved-cont)
        (apply-cont saved-cont
          (bool-val (not (expval->bool val)))
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
      (rator-cont (rands saved-env saved-cont)
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
      (rand1-cont (rands proc1 saved-env saved-cont)
        (value-of/k (car rands) saved-env
          (cases proc proc1
            (procedure (vars body p-saved-env)
              (let (
                  [rds (cdr rands)]
                  [proc2 (procedure (cdr vars) body (extend-env (car vars) (newref val) p-saved-env))]
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
      (rand2-cont (proc1 saved-cont)
        (apply-procedure/k proc1 (list val) saved-cont)
      )
    )
  )
)

; Stmt * Env * CmdCont -> ()
(define result-of
  (lambda (stmt env cmd-cont)
    (cases statement stmt
      (assign-stmt (var exp1)
        (value-of/k exp1 env (assign-cont var env cmd-cont))
      )
      (print-stmt (exp1)
        (value-of/k exp1 env (print-cont cmd-cont))
      )
      (begin-stmt (stmts)
        (if (null? stmts)
          (apply-cmd-cont cmd-cont)
          (result-of (car stmts) env (begin-cmd-cont (cdr stmts) env cmd-cont))
        )
      )
      (if-stmt (exp1 stmt1 stmt2)
        (value-of/k exp1 env (if-test-cont stmt1 stmt2 env cmd-cont))
      )
      (while-stmt (exp1 stmt1)
        (value-of/k exp1 env (while-test-cont exp1 stmt1 env cmd-cont))
      )
      (var-stmt (vars stmt1)
        (result-of stmt1
          (let f ([vrs vars])
            (if (null? vrs)
              env
              (extend-env (car vrs) (newref (num-val 0)) (f (cdr vrs)))
            )
          )
          cmd-cont
        )
      )
    )
  )
)

; Exp x Env * Cont -> ExpVal
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (deref (apply-env env var))))
      (proc-exp (vars body)
        (apply-cont cont
          (proc-val (procedure vars body env))
        )
      )
      (zero?-exp (exp1)
        (value-of/k exp1 env (zero1-cont cont))
      )
      (not-exp (exp1)
        (value-of/k exp1 env (not-cont cont))
      )
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (binary-op1-cont - exp2 env cont)
        )
      )
      (add-exp (exp1 exp2)
        (value-of/k exp1 env
          (binary-op1-cont + exp2 env cont)
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
            (extend-env (car vars) (newref (car vals)) saved-env)
          )
          cont
        )
      )
    )
  )
)
