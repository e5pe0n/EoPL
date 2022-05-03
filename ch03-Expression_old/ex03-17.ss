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
    (vars list?)
    (exps list?)
    (body expression?)
  )
  (let*-exp
    (vars list?)
    (exps list?)
    (body expression?)
  )
  (minus-exp
    (exp1 expression?)
  )
  (list-exp
    (exp1 list-expression?)
  )
  (null?-exp
    (exp1 expression?)
  )
  (car-exp
    (exp1 list-expression?)
  )
  (unpack-exp
    (vars list?)
    (exp1 list-expression?)
    (body expression?)
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

(define-datatype list-expression list-expression?
  (emptylist-exp)
  (cons-exp
    (first expression?)
    (rest list-expression?)
  )
  (cdr-exp
    (exp1 list-expression?)
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
)

(define vlist?
  (lambda (x)
    (or (null? x) (pair? x))
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
      (let-exp (vars exps body)
        (value-of body
          (let f ([vs vars] [es exps])
            (if (null? vs)
              env
              (extend-env
                (car vs)
                (value-of (car es) env)
                (f (cdr vs) (cdr es))
              )
            )
          )
        )
      )
      (let*-exp (vars exps body)
        (if (null? vars)
          (value-of body env)
          (value-of
            (let*-exp (cdr vars) (cdr exps) body)
            (extend-env
              (car vars)
              (value-of (car exps) env)
              env
            )
          )
        )
      )
      (minus-exp (exp1)
        (value-of (diff-exp (const-exp 0) exp1) env)
      )
      (list-exp (exp1)
        (value-of-list-exp exp1 env)
      )
      (null?-exp (exp1)
        (cases expval (value-of exp1 env)
          (num-val (num) #f)
          (boolval (bool) #f)
          (list-val (lst1) (null? lst1))
        )
      )
      (car-exp (exp1)
        (cases list-expression exp1
          (cons-exp (first rest)
            (value-of first env)
          )
        )
      )
      (unpack-exp (vars exp1 body)
        (if (null? vars)
          (value-of body env)
          (value-of
            (unpack-exp
              (cdr vars)
              (cases list-expression exp1
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

(define value-of-list-exp
  (lambda (exp env)
    (cases list-expression exp
      (emptylist-exp () (list-val '()))
      (cons-exp (first rest)
        (list-val
          (cons
            (value-of first env)
            (value-of-list-exp rest env)
          )
        )
      )
      (cdr-exp (exp1)
        (cases list-expression exp1
          (cons-exp (first rest)
            (value-of-list-exp rest env)
          )
        )
      )
    )
  )
)

(print
  (expval->num
    (value-of
      (let-exp
        (list 'u)
        (list (const-exp 7))
        (unpack-exp
          (list 'x 'y)
          (cons-exp
            (var-exp 'u)
            (cons-exp
              (const-exp 3)
              (emptylist-exp)
            )
          )
          (diff-exp (var-exp 'x) (var-exp 'y))
        )
      )
      (empty-env)
    )
  )
) ; 4
; (print
;   (expval->num
;     (value-of
;       (let-exp
;         (list 'x)
;         (list (const-exp 30))
;         (let-exp
;           (list 'x 'y)
;           (list
;             (diff-exp (var-exp 'x) (const-exp 1))
;             (diff-exp (var-exp 'x) (const-exp 2))
;           )
;           (diff-exp (var-exp 'x) (var-exp 'y))
;         )
;       )
;       (empty-env)
;     )
;   )
; ) ; 1
; (print
;   (expval->num
;     (value-of
;       (let-exp
;         (list 'x)
;         (list (const-exp 30))
;         (let*-exp
;           (list 'x 'y)
;           (list
;             (diff-exp (var-exp 'x) (const-exp 1))
;             (diff-exp (var-exp 'x) (const-exp 2))
;           )
;           (diff-exp (var-exp 'x) (var-exp 'y))
;         )
;       )
;       (empty-env)
;     )
;   )
; ) ; 2
; (print
;   (expval->list
;     (value-of-list-exp
;       (cons-exp
;         (list-exp
;           (cons-exp
;             (const-exp 10)
;             (cons-exp
;               (const-exp 11)
;               (emptylist-exp)
;             )
;           )
;         )
;         (cons-exp
;           (const-exp 2)
;           (cons-exp
;             (const-exp 1)
;             (emptylist-exp)
;           )
;         )
;       )
;       (empty-env)
;     )
;   )
; )