(import datatype)

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (saved-var symbol?)
    (saved-val s-val?)
    (saved-env environment?)
  )
  (extend-env-rec
    (p-name identifier?)
    (b-var identifier?)
    (body expression?)
    (env environment?)
  )
)
(define s-val?
  (lambda (x)
    #t
  )
)
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env () (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)
        )
      )
      (extend-env-rec (p-name b-var p-body saved-env)
        (if (eqv? p-name search-var)
          (proc-val (procedure b-var p-body env))
          (apply-env saved-env search-var)
        )
      )
    )
  )
)
(define has-binding?
  (lambda (env search-var)
    (cases environment env
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
    (error 'apply-env ": No binding for " search-var)
  )
)
(define report-invalid-env
  (lambda (env)
    (error 'apply-env ": Bad environment=" env)
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
  (letrec-exp
    (p-name identifier?)
    (param identifier?)
    (p-body expression?)
    (letrec-body expression?)
  )
  (minus-exp
    (exp1 expression?)
  )
  (null?-exp
    (exp1 expression?)
  )
  (emptylist-exp)
  (cons-exp
    (first expression?)
    (rest expression?)
  )
  (cdr-exp
    (exp1 expression?)
  )
  (car-exp
    (exp1 expression?)
  )
  (unpack-exp
    (vars (list-of identifier?))
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

  (not-exp
    (exp1 expression?)
  )
  (and-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (or-exp
    (exp1 expression?)
    (exp2 expression?)
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
    (saved-env environment?)
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
  (list-val
    (lst1 vlist?)
  )
  (proc-val
    (proc1 proc?)
  )
)

(define vlist?
  (lambda (x)
    (or (null? x) (pair? x))
  )
)
(define list->list-exp
  (lambda (x)
    (if (null? x)
      (emptylist-exp)
      (cons-exp (car x) (list->list-exp (cdr x)))
    )
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
(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (lst1)
        (if (null? lst1)
          '()
          (cons
            (cases expval (car lst1)
              (num-val (num) num)
              (bool-val (bool) bool)
              (list-val (_lst1) (expval->list (car lst1)))
            )
            (expval->list (cdr lst1))
          )
        )
      )
    )
  )
)
(define expval->car
  (lambda (val)
    (cases expval val
      (list-val (lst1)
        (car lst1)
      )
    )
  )
)
(define expval->cdr
  (lambda (val)
    (cases expval val
      (list-val (lst1)
        (cdr lst1)
      )
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
      (bool-exp (bool) (bool-val bool))
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
        (let ([val1 (value-of exp1 env)])
          (if (expval->bool val1)
            (value-of exp2 env)
            (value-of exp3 env)
          )
        )
      )
      (let-exp (var exp1 body)
        (value-of
          body
          (extend-env var (value-of exp1 env) env)
        )
      )
      (letrec-exp (p-name param p-body letrec-body)
        (value-of
          letrec-body
          (extend-env-rec p-name param p-body env)
        )
      )
      (minus-exp (exp1)
        (value-of (diff-exp (const-exp 0) exp1) env)
      )
      (null?-exp (exp1)
        (cases expval (value-of exp1 env)
          (num-val (num) #f)
          (boolval (bool) #f)
          (list-val (lst1) (null? lst1))
        )
      )
      (car-exp (exp1)
        (cases expression exp1
          (cons-exp (first rest)
            (value-of first env)
          )
        )
      )
      (cons-exp (first rest)
        (list-val
          (cons
            (value-of first env)
            (value-of rest env)
          )
        )
      )
      (cdr-exp (exp1)
        (cases expression exp1
          (cons-exp (first rest)
            (value-of rest env)
          )
        )
      )
      (unpack-exp (vars exp1 body)
        (if (null? vars)
          (value-of body env)
          (value-of
            (unpack-exp
              (cdr vars)
              (cases expression exp1
                (cons-exp (first rest)
                  rest
                )
              )
              body
            )
            (extend-env
              (car vars)
              (value-of (car-exp exp1) env)
              env
            )
          )
        )
      )
      (proc-exp (param body)
        (proc-val (procedure param body env))
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

      (not-exp (exp1)
        (bool-val (not (expval->bool (value-of exp1 env))))
      )
      (and-exp (exp1 exp2)
        (bool-val (and
          (expval->bool (value-of exp1 env))
          (expval->bool (value-of exp2 env))
        ))
      )
      (or-exp (exp1 exp2)
        (bool-val (or
          (expval->bool (value-of exp1 env))
          (expval->bool (value-of exp2 env))
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
        (value-of (zero?-exp (diff-exp exp1 exp2)) env)
      )
      (less?-exp (exp1 exp2)
        (let ([val1 (value-of exp1 env)] [val2 (value-of exp2 env)])
          (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
            (bool-val (< num1 num2))
          )
        )
      )
      (greater?-exp (exp1 exp2)
        (value-of
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
      (let-exp 'fact
        (proc-exp 'n (diff-exp (var-exp 'n) (const-exp -1)))
        (let-exp 'fact
          (proc-exp 'n
            (if-exp (zero?-exp (var-exp 'n))
              (const-exp  1)
              (mul-exp
                (var-exp 'n)
                (call-exp
                  (var-exp 'fact)
                  (diff-exp (var-exp 'n) (const-exp 1))
                )
              )
            )
          )
          (call-exp (var-exp 'fact) (const-exp 5))
        )
      )
      (empty-env)
    )
  )
) ; 25