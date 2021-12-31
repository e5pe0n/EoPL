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
)
(define identifier?
  (lambda (x)
    (symbol? x)
  )
)


(define-datatype expval expval?
  (num-val
    (num number?)
  )
  (bool-val
    (bool boolean?)
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
(define report-expval-extractor-error
  (lambda (name val)
    (error name "invalid syntax: -s" val)
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
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
        (let
          (
            [val1 (value-of exp1 env)]
            [val2 (value-of exp2 env)]
          )
          (let
            (
              [num1 (expval->num val1)]
              [num2 (expval->num val2)]
            )
            (num-val (- num1 num2))
          )
        )
      )
      (zero?-exp (exp1)
        (let ([val1 (value-of exp1 env)])
          (let ([num1 (expval->num val1)])
            (if (zero? num1)
              (bool-val #t)
              (bool-val #f)
            )
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (let ([val1 (value-of exp1 env)])
          (if (expval->bool val1)
            (value-of exp2 env)
            (value-of exp3 env)
          )
        )
      )
      (let-exp (var exp1 body)
        (let ([val1 (value-of exp1 env)])
          (value-of body
            (extend-env var val1 env)
          )
        )
      )
    )
  )
)

(define p1
  (a-program
    (diff-exp
      (diff-exp (var-exp 'x) (const-exp 3))
      (diff-exp (var-exp 'v) (var-exp 'i))
    )
  )
)
(print
  (expval->num
    (value-of-program p1)
  )
) ; 3

(define e2
  (if-exp (zero?-exp (diff-exp (var-exp 'x) (const-exp 11)))
    (diff-exp (var-exp 'y) (const-exp 2))
    (diff-exp (var-exp 'y) (const-exp 4))
  )
)
(define env2
  (extend-env 'x (num-val 33)
    (extend-env 'y (num-val 22)
      (empty-env)
    )
  )
)
(print
  (expval->num
    (value-of e2 env2)
  )
) ; 18

(define e3
  (let-exp 'x (const-exp 7)
    (let-exp 'y (const-exp 2)
      (let-exp
        'y
        (let-exp
          'x
          (diff-exp (var-exp 'x) (const-exp 1))
          (diff-exp (var-exp 'x) (var-exp 'y))
        )
        (diff-exp
          (diff-exp (var-exp 'x) (const-exp 8))
          (var-exp 'y)
        )
      )
    )
  )
)
(print
  (expval->num
    (value-of e3 (empty-env))
  )
) ; 5