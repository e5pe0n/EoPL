#lang eopl

(require "data-structures.rkt")
(require "utils.rkt")
(require "lang.rkt")

(provide (all-defined-out))

(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)

; FinalAnswer = ExpVal
; Cont = ExpVal -> FinalAnswer

; Program -> FinalAnswer
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (set! cont (end-cont))
        (set! exp exp1)
        (set! env (init-env))
        (value-of/k)
      )
    )
  )
)

(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont (cont continuation?))
  (let-exp-cont
    (var identifier?)
    (body expression?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (saved-env environment?)
    (saved-cont continuation?)
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
)

; () -> FinalAnswer
; exp: Exp
; env: Env
; cont: Cont
(define value-of/k
  (lambda ()
    (cases expression exp
      (const-exp (num)
        (set! val (num-val num))
        (apply-cont)
      )
      (var-exp (var)
        (set! val (apply-env env var))
        (apply-cont)
      )
      (proc-exp (var body)
        (set! val (proc-val (procedure var body env)))
        (apply-cont)
      )
      (letrec-exp (p-name b-var p-body letrec-body)
        (set! exp letrec-body)
        (set! env (extend-env-rec p-name b-var p-body env))
        (value-of/k)
      )
      (zero?-exp (exp1)
        (set! cont (zero1-cont cont))
        (set! exp exp1)
        (value-of/k)
      )
      (let-exp (var exp1 body)
        (set! cont (let-exp-cont var body env cont))
        (set! exp exp1)
        (value-of/k)
      )
      (if-exp (exp1 exp2 exp3)
        (set! cont (if-test-cont exp2 exp3 env cont))
        (set! exp exp1)
        (value-of/k)
      )
      (diff-exp (exp1 exp2)
        (set! cont (binary-op1-cont - exp2 env cont))
        (set! exp exp1)
        (value-of/k)
      )
      (mul-exp (exp1 exp2)
        (set! cont (binary-op1-cont * exp2 env cont))
        (set! exp exp1)
        (value-of/k)
      )
      (call-exp (rator rand)
        (set! cont (rator-cont rand env cont))
        (set! exp rator)
        (value-of/k)
      )
    )
  )
)

; () -> FinalAnswer
; cont: Cont
; val: ExpVal
(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont ()
        (println "End of computation.")
        val
      )
      (zero1-cont (saved-cont)
        (set! cont saved-cont)
        (set! val (bool-val (zero? (expval->num val))))
        (apply-cont)
      )
      (let-exp-cont (var body saved-env saved-cont)
        (set! cont saved-cont)
        (set! exp body)
        (set! env (extend-env var val saved-env))
        (value-of/k)
      )
      (if-test-cont (exp2 exp3 saved-env saved-cont)
        (set! cont saved-cont)
        (set! exp
          (if (expval->bool val)
            exp2
            exp3
          )
        )
        (set! env saved-env)
        (value-of/k)
      )
      (binary-op1-cont (op exp2 saved-env saved-cont)
        (set! cont (binary-op2-cont op val saved-cont))
        (set! exp exp2)
        (set! env saved-env)
        (value-of/k)
      )
      (binary-op2-cont (op val1 saved-cont)
        (set! cont saved-cont)
        (set! val (num-val (op (expval->num val1) (expval->num val))))
        (apply-cont)
      )
      (rator-cont (rand saved-env saved-cont)
        (set! cont (rand-cont val saved-cont))
        (set! exp rand)
        (set! env saved-env)
        (value-of/k)
      )
      (rand-cont (val1 saved-cont)
        (set! cont saved-cont)
        (cases proc (expval->proc val1)
          (procedure (var body saved-env)
            (set! exp body)
            (set! env (extend-env var val saved-env))
          )
        )
        (apply-procedure/k)
      )
    )
  )
)

; () -> FinalAnswer
; val: ExpVal
; cont: Cont
(define apply-procedure/k
  (lambda ()
    (value-of/k)
  )
)
