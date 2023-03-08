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
        (value-of/k exp1 (init-env)
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
  )
)

; FinalAnswer = ExpVal

; Exp x Env * Cont * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont exc-cont)
    (cases expression exp
      (const-exp (num) (cont (num-val num)))
      (var-exp (var) (cont (apply-env env var)))
      (proc-exp (var body)
        (cont
          (proc-val (procedure var body env))
        )
      )
      (letrec-exp (p-name b-var p-body letrec-body)
        (value-of/k
          letrec-body
          (extend-env-rec p-name b-var p-body env)
          cont
          exc-cont
        )
      )
      (zero?-exp (exp1)
        (value-of/k exp1 env
          (lambda (val)
            (cont
              (bool-val (zero? (expval->num val)))
            )
          )
          (lambda (val)
            (exc-cont val)
          )
        )
      )
      (let-exp (var exp1 body)
        (value-of/k exp1 env
          (lambda (val)
            (value-of/k body (extend-env var val env) cont exc-cont)
          )
          (lambda (val)
            (exc-cont val)
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (lambda (val)
            (if (expval->bool val)
              (value-of/k exp2 env cont exc-cont)
              (value-of/k exp3 env cont exc-cont)
            )
          )
          (lambda (val)
            (exc-cont val)
          )
        )
      )
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (lambda (val1)
            (value-of/k exp2 env
              (lambda (val2)
                (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
                  (cont
                    (num-val (- num1 num2))
                  )
                )
              )
              (lambda (val)
                (exc-cont val)
              )
            )
          )
          (lambda (val)
            (exc-cont val)
          )
        )
      )
      (mul-exp (exp1 exp2)
        (value-of/k exp1 env
          (lambda (val1)
            (value-of/k exp2 env
              (lambda (val2)
                (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
                  (cont
                    (num-val (* num1 num2))
                  )
                )
              )
              (lambda (val)
                (exc-cont val)
              )
            )
          )
          (lambda (val)
            (exc-cont val)
          )
        )
      )
      (div-exp (exp1 exp2)
        (value-of/k exp1 env
          (lambda (val1)
            (value-of/k exp2 env
              (lambda (val2)
                (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
                  (if (zero? num2)
                    (exc-cont (num-val 101))
                    (cont (num-val (/ num1 num2)))
                  )
                )
              )
              (lambda (val)
                (exc-cont val)
              )
            )
          )
          (lambda (val)
            (exc-cont val)
          )
        )
      )
      (call-exp (rator rand)
        (value-of/k rator env
          (lambda (val1)
            (value-of/k rand env
              (lambda (val2)
                (let ([proc1 (expval->proc val1)])
                  (apply-procedure/k proc1 val2 cont exc-cont)
                )
              )
              (lambda (val)
                (exc-cont val)
              )
            )
          )
          (lambda (val)
            (exc-cont val)
          )
        )
      )
      (try-exp (exp1 var handler-exp)
        (value-of/k exp1 env
          (lambda (val)
            (cont val)
          )
          (lambda (val)
            (value-of/k handler-exp
              (extend-env var val env)
              cont
              exc-cont
            )
          )
        )
      )
      (raise-exp (exp1)
        (value-of/k exp1 env
          (lambda (val)
            (exc-cont val)
          )
          (lambda (val)
            (exc-cont val)
          )
        )
      )
    )
  )
)

; Proc * ExpVal * Cont * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 val cont exc-cont)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of/k body (extend-env var val saved-env) cont exc-cont)
      )
    )
  )
)
