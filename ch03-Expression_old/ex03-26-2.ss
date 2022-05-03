(import datatype)

(define-datatype env env?
  (empty-env)
  (extend-env
    (saved-var symbol?)
    (saved-val s-val?)
    (saved-env env?)
  )
)
(define s-val?
  (lambda (x)
    #t
  )
)
(define apply-env
  (lambda (e search-var)
    (cases env e
      (empty-env () (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)
        )
      )
    )
  )
)
(define free-env
  (lambda (e search-var)
    (cases env e
      (empty-env () (empty-env))
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? search-var saved-var)
          (free-env saved-env search-var)
          (extend-env saved-var saved-val (free-env saved-env search-var))
        )
        
      )
    )
  )
)
(define has-binding?
  (lambda (e search-var)
    (cases env e
      (empty-env () #f)
      (extend-env (saved-var saved-val saved-env)
        (or (eqv? search-var saved-var)
          (has-binding? saved-env search-var)
        )
      )
    )
  )
)
(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env ": No binding for -s" search-var)
  )
)
(define report-invalid-env
  (lambda (e)
    (error 'apply-env ": Bad environment=-s" e)
  )
)

(define-datatype program program?
  (a-program
    (exp1 expression?)
  )
)
(define-datatype expression expression?
  (const-exp
    (num number?)
  )
  (diff-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (zero?-exp
    (exp1 expression?)
  )
  (if-exp
    (exp1 expression?)
    (exp2 expression?)
    (exp3 expression?)
  )
  (var-exp
    (var identifier?)
  )
  (let-exp
    (var identifier?)
    (exp1 expression?)
    (body expression?)
  )
  (proc-exp
    (param identifier?)
    (body expression?)
  )
  (call-exp
    (rator expression?)
    (rand expression?)
  )
)
(define identifier?
  (lambda (x)
    (symbol? x)
  )
)
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
        (and (pred (car val)) ((list-of pred) (cdr val)))
      )
    )
  )
)


(define-datatype proc proc?
  (procedure
    (param identifier?)
    (body expression?)
    (saved-env env?)
  )
)

(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
      (procedure (param body saved-env)
        (value-of
          body
          (extend-env param arg saved-env)
        )
      )
    )
  )
)


(define-datatype expval expval?
  (num-val
    (num number?)
  )
  (bool-val
    (bool boolean?)
  )
  (proc-val
    (proc1 proc?)
  )
)


(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else report-expval-extractor-error 'num val)
    )
  )
)
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else report-expval-extractor-error 'bool val)
    )
  )
)
(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc1) proc1)
    )
  )
)
(define report-expval-extractor-error
  (lambda (name val)
    (error name "invalid syntax: ~s" val)
  )
)

(define init-env
  (lambda ()
    (extend-env 'i (num-val 1)
      (extend-env 'v (num-val 5)
        (extend-env 'x (num-val 10)
          (empty-env)
        )
      )
    )
  )
)

(define run
  (lambda (string)
    (value-of-program (span&parse string))
  )
)
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))
      )
    )
  )
)
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (bool-exp (bool) (bool-val bool))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
        (num-val
          (-
            (expval->num (value-of exp1 env))
            (expval->num (value-of exp2 env))
          )
        )
      )
      (zero?-exp (exp1)
        (zero? (expval->num (value-of exp1 env)))
      )
      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env))
          (value-of exp2 env)
          (value-of exp3 env)
        )
      )
      (let-exp (var exp1 body)
        (value-of
          body
          (extend-env var (value-of exp1 env) env)
        )
      )
      (proc-exp (param body)
        (proc-val (procedure param body (free-env env param)))
      )
      (call-exp (rator rand)
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
)

(print
  (expval->num
    (value-of
      (let-exp 'x (const-exp 1)
        (let-exp 'y (const-exp 2)
          (let-exp 'f (proc-exp 'x (diff-exp (var-exp 'x) (var-exp 'y)))
            (let-exp 'y (const-exp 4)
              (call-exp (var-exp 'f) (var-exp 'y))
            )
          )
        )
      )
      (empty-env)
    )
  )
) ; 2
