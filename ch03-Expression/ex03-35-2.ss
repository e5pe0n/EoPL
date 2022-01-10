(define empty-env
  (lambda ()
    (lambda (search-var)
      (report-no-binding-found search-var)
    )
  )
)
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
        (if (vector? saved-val)
          (vector-ref saved-val 0)
          saved-val
        )
        (apply-env saved-env search-var)
      )
    )
  )
)
(define extend-env-rec
  (lambda (p-name b-var body saved-env)
    (let ([vec (make-vector 1)])
      (let ([new-env (extend-env p-name vec saved-env)])
        (vector-set! vec 0 (proc-val (procedure b-var body new-env)))
        new-env
      )
    )
  )
)
(define apply-env
  (lambda (env search-var)
    (env search-var)
  )
)

(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env ": No binding for " search-var)
  )
)
(define report-invalid-env
  (lambda (env)
    (error 'apply-env ": Bad environment=" env)
  )
)

(define procedure
  (lambda (param body saved-env)
    (lambda (arg)
      (value-of
        body
        (extend-env param arg saved-env)
      )
    )
  )
)
(define apply-procedure
  (lambda (proc1 arg)
    (proc1 arg)
  )
)


(define num-val
  (lambda (num)
    (lambda (type)
      (if (eqv? type 'num)
        num
        (report-expval-extractor-error 'num val)
      )
    )
  )
)
(define bool-val
  (lambda (bool)
    (lambda (type)
      (if (eqv? type 'bool)
        bool
        (report-expval-extractor-error 'bool val)
      )
    )
  )
)
(define proc-val
  (lambda (proc1)
    (lambda (type)
      (if (eqv? type 'proc)
        proc1
        (report-expval-extractor-error 'proc val)
      )
    )
  )
)


(define expval->num
  (lambda (val)
    (val 'num)
  )
)
(define expval->bool
  (lambda (val)
    (val 'bool)
  )
)
(define expval->proc
  (lambda (val)
    (val 'proc)
  )
)


(define const-exp
  (lambda (num)
    (lambda (env)
      (num-val num)
    )
  )
)
(define bool-exp
  (lambda (bool)
    (lambda (env)
      (bool-val bool)
    )
  )
)
(define var-exp
  (lambda (var)
    (lambda (env)
      (apply-env env var)
    )
  )
)
(define diff-exp
  (lambda (exp1 exp2)
    (lambda (env)
      (let
        (
          [num1 (expval->num (value-of exp1 env))]
          [num2 (expval->num (value-of exp2 env))]
        )
        (num-val (- num1 num2))
      )
    )
  )
)
(define minus-exp
  (lambda (exp1)
    (lambda (env)
      (value-of (diff-exp (const-exp 0) exp1) env)
    )
  )
)
(define if-exp
  (lambda (exp1 exp2 exp3)
    (lambda (env)
      (if (expval->bool (value-of exp1 env))
        (value-of exp2 env)
        (value-of exp3 env)
      )
    )
  )
)
(define zero?-exp
  (lambda (exp1)
    (lambda (env)
      (bool-val (zero? (expval->num (value-of exp1 env))))
    )
  )
)
(define equal?-exp
  (lambda (exp1 exp2)
    (lambda (env)
      (value-of (zero?-exp (diff-exp exp1 exp2)) env)
    )
  )
)
(define let-exp
  (lambda (var exp1 body)
    (lambda (env)
      (value-of
        body
        (extend-env var (value-of exp1 env) env)
      )
    )
  )
)
(define letrec-exp
  (lambda (p-name param p-body letrec-body)
    (lambda (env)
      (value-of
        letrec-body
        (extend-env-rec p-name param p-body env)
      )
    )
  )
)
(define proc-exp
  (lambda (param body)
    (lambda (env)
      (proc-val (procedure param body env))
    )
  )
)
(define call-exp
  (lambda (rator rand)
    (lambda (env)
      (let
        (
          [proc1 (expval->proc (value-of rator env))]
          [arg (value-of rand env)]
        )
        (apply-procedure proc1 arg)
      )
    )
  )
)


(define value-of
  (lambda (exp env)
    (exp env)
  )
)



(print
  (expval->num
    (value-of
      (letrec-exp 'f 'x
        (if-exp (zero?-exp (var-exp 'x))
          (const-exp 0)
          (diff-exp
            (call-exp (var-exp 'f) (diff-exp (var-exp 'x) (const-exp 1)))
            (minus-exp (const-exp 2))
          )
        )
        (call-exp (var-exp 'f) (const-exp 6))
      )
      (empty-env)
    )
  )
) ; 12