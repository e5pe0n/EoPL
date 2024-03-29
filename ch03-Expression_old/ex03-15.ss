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
  (add-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (mul-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (quo-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (if-exp
    (exp1 bool-expression?)
    (exp2 expression?)
    (exp3 expression?)
  )
  (var-exp
    (var identifier?)
  )
  (let-exp
    (var-exp-pairs list?)
    (body expression?)
  )
  (let*-exp
    (var-exp-pairs list?)
    (body expression?)
  )
  (minus-exp
    (exp1 expression?)
  )
)
(define identifier?
  (lambda (x)
    (symbol? x)
  )
)

(define-datatype bool-expression bool-expression?
  (bool-exp
    (bool boolean?)
  )
  (not-exp
    (exp1 bool-expression?)
  )
  (and-exp
    (exp1 bool-expression?)
    (exp2 bool-expression?)
  )
  (zero?-exp
    (exp1 expression?)
  )
  (equal?-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (less?-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (greater?-exp
    (exp1 expression?)
    (exp2 expression?)
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
      (add-exp (exp1 exp2)
        (value-of (diff-exp exp1 (minus-exp exp2)) env)
      )
      (mul-exp (exp1 exp2)
        (value-of
          (let-exp 'expression1 (if-exp (less?-exp exp1 exp2) exp1 exp2)
            (let-exp 'expression2 (if-exp (less?-exp exp1 exp2) exp2 exp1)
              (if-exp (zero?-exp (var-exp 'expression1))
                (const-exp 0)
                (add-exp (var-exp 'expression2) (mul-exp (diff-exp (var-exp 'expression1) (const-exp 1)) (var-exp 'expression2)))
              )
            )
          )
          env
        )
      )
      (quo-exp (exp1 exp2)
        (value-of
          (if-exp (less?-exp exp1 exp2)
            (const-exp 0)
            (add-exp (const-exp 1) (quo-exp (diff-exp exp1 exp2) exp2))
          )
          env
        )
      )
      (if-exp (exp1 exp2 exp3)
        (let ([val1 (value-of-bool-exp exp1 env)])
          (if (expval->bool val1)
            (value-of exp2 env)
            (value-of exp3 env)
          )
        )
      )
      (let-exp (var-exp-pairs body)
        (value-of body
          (let f ([lst var-exp-pairs])
            (if (null? lst)
              env
              (let ([fst (car lst)])
                (extend-env (car fst) (value-of (cadr fst) env) (f (cdr lst)))
              )
            )
          )
        )
      )
      (let*-exp (var-exp-pairs body)
        (if (null? var-exp-pairs)
          (value-of body env)
          (value-of
            (let-exp (cdr var-exp-pairs) body)
            (let ([fst (car var-exp-pairs)])
              (extend-env (car fst) (value-of (cadr fst) env) env)
            )
          )
        )
      )
      (minus-exp (exp1)
        (value-of (diff-exp (const-exp 0) exp1) env)
      )
    )
  )
)
(define value-of-bool-exp
  (lambda (exp env)
    (cases bool-expression exp
      (bool-exp (bool) (bool-val bool))
      (not-exp (exp1)
        (bool-val (not (expval->bool (value-of-bool-exp exp1 env))))
      )
      (and-exp (exp1 exp2)
        (bool-val (and
          (expval->bool (value-of-bool-exp exp1 env))
          (expval->bool (value-of-bool-exp exp2 env))
        ))
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
      (equal?-exp (exp1 exp2)
        (value-of-bool-exp (zero?-exp (diff-exp exp1 exp2)) env)
      )
      (less?-exp (exp1 exp2)
        (let ([val1 (value-of exp1 env)] [val2 (value-of exp2 env)])
          (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
            (bool-val (< num1 num2))
          )
        )
      )
      (greater?-exp (exp1 exp2)
        (value-of-bool-exp
          (and-exp
            (not-exp (less?-exp exp1 exp2))
            (not-exp (equal?-exp exp1 exp2))
          )
          env
        )
      )
    )
  )
)

(print
  (expval->num
    (value-of
      (let-exp
        (list (list 'x (const-exp 30)))
        (let-exp
          (list
            (list 'x (diff-exp (var-exp 'x) (const-exp 1)))
            (list 'y (diff-exp (var-exp 'x) (const-exp 2)))
          )
          (diff-exp (var-exp 'x) (var-exp 'y))
        )
      )
      (empty-env)
    )
  )
) ; 1
(print
  (expval->num
    (value-of
      (let-exp
        (list (list 'x (const-exp 30)))
        (let*-exp
          (list
            (list 'x (diff-exp (var-exp 'x) (const-exp 1)))
            (list 'y (diff-exp (var-exp 'x) (const-exp 2)))
          )
          (diff-exp (var-exp 'x) (var-exp 'y))
        )
      )
      (empty-env)
    )
  )
) ; 2