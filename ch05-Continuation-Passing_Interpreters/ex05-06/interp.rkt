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
    (var identifier?)
    (body expression?)
    (env environment?)
    (cont continuation?)
  )
  (let2-exp1-cont
    (var1 identifier?)
    (var2 identifier?)
    (exp2 expression?)
    (body expression?)
    (env environment?)
    (cont continuation?)
  )
  (let2-exp2-cont
    (var1 identifier?)
    (var2 identifier?)
    (val1 expval?)
    (body expression?)
    (env environment?)
    (cont continuation?)
  )
  (let3-exp1-cont
    (var1 identifier?)
    (var2 identifier?)
    (var3 identifier?)
    (exp2 expression?)
    (exp3 expression?)
    (body expression?)
    (env environment?)
    (cont continuation?)
  )
  (let3-exp2-cont
    (var1 identifier?)
    (var2 identifier?)
    (var3 identifier?)
    (val1 expval?)
    (exp3 expression?)
    (body expression?)
    (env environment?)
    (cont continuation?)
  )
  (let3-exp3-cont
    (var1 identifier?)
    (var2 identifier?)
    (var3 identifier?)
    (val1 expval?)
    (val2 expval?)
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
      (let-exp-cont (var body saved-env saved-cont)
        (value-of/k body
          (extend-env var val saved-env)
          saved-cont
        )
      )
      (let2-exp1-cont (var1 var2 exp2 body saved-env saved-cont)
        (value-of/k exp2
          saved-env
          (let2-exp2-cont var1 var2 val body saved-env saved-cont)
        )
      )
      (let2-exp2-cont (var1 var2 val1 body saved-env saved-cont)
        (value-of/k body
          (extend-env var1 val1
            (extend-env var2 val saved-env)
          )
          saved-cont
        )
      )
      (let3-exp1-cont (var1 var2 var3 exp2 exp3 body saved-env saved-cont)
        (value-of/k exp2
          saved-env
          (let3-exp2-cont var1 var2 var3 val exp3 body saved-env saved-cont)
        )
      )
      (let3-exp2-cont (var1 var2 var3 val1 exp3 body saved-env saved-cont)
        (value-of/k exp3
          saved-env
          (let3-exp3-cont var1 var2 var3 val1 val body saved-env saved-cont)
        )
      )
      (let3-exp3-cont (var1 var2 var3 val1 val2 body saved-env saved-cont)
        (value-of/k body
          (extend-env var1 val1
            (extend-env var2 val2
              (extend-env var3 val saved-env)
            )
          )
          saved-cont
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
      (let-exp (var exp1 body)
        (value-of/k exp1 env
          (let-exp-cont var body env cont)
        )
      )
      (let2-exp (var1 exp1 var2 exp2 body)
        (value-of/k exp1 env
          (let2-exp1-cont var1 var2 exp2 body env cont)
        )
      )
      (let3-exp (var1 exp1 var2 exp2 var3 exp3 body)
        (value-of/k exp1 env
          (let3-exp1-cont var1 var2 var3 exp2 exp3 body env cont)
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
