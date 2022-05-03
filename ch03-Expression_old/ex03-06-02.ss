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
    (error 'apply-env ": No binding for ~s" search-var)
  )
)
(define report-invalid-env
  (lambda (e)
    (error 'apply-env ": Bad environment=~s" e)
  )
)

(define-datatype program program?
  (a-program
    (exp1 expression?)
  )
)

(define list-of
  (lambda (pred)
    (lambda (list1)
      (if (null? list1)
        #t
        (and
          (pred (car list1))
          ((list-of pred) (cdr list1))
        )
      )
    )
  )
)

(define-datatype expression expression?
  (const-exp
    (num number?)
  )
  (bool-exp
    (bool boolean?)
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
  (not-exp
    (exp1 expression?)
  )
  (emptylist-exp)
  (list-exp
    (exps (list-of expression?))
  )
  (cons-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (car-exp
    (exp1 expression?)
  )
  (cdr-exp
    (exp1 expression?)
  )
  (cond-exp
    (exp-pairs list?)
  )
)
(define identifier?
  (lambda (x)
    (symbol? x)
  )
)

(define report-empty-list-error
  (lambda (name)
    (error name "empty list")
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
    (list1 list?)
  )
)
(define any->expval
  (lambda (x)
    (cond
      ((number? x) (num-val x))
      ((boolean? x) (bool-val x))
      ((list? x) (list-val x))
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
      (list-val (list1) list1)
      (else report-expval-extractor-error 'list val)
    )
  )
)
(define expval->any
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (bool-val (bool) bool)
      (list-val (list1) list1)
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
          (- (expval->num (value-of exp1 env)) (expval->num (value-of exp2 env)))
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
        (value-of body
          (extend-env var (value-of exp1 env) env)
        )
      )
      (not-exp (exp1)
        (bool-val (not (expval->bool (value-of exp1 env))))
      )
      (emptylist-exp () (list-val '()))
      (list-exp (list1)
        (list-val
          (let f ([lst1 list1])
            (if (null? lst1)
              '()
              (cons
                (expval->any (value-of (car lst1) env))
                (f (cdr lst1))
              )
            )
          )
        )
      )
      (cons-exp (exp1 exp2)
        (list-val
          (cons
            (expval->any (value-of exp1 env))
            (expval->list (value-of exp2 env))
          )
        )
      )
      (car-exp (exp1)
        (any->expval
          (car (expval->list (value-of exp1 env)))
        )
      )
      (cdr-exp (exp1)
        (list-val
          (cdr (expval->list (value-of exp1 env)))
        )
      )
      (cond-exp (exp-pairs)
        (if (null? exp-pairs)
          (error "value-of cond-exp; no satisfied condition")
          (let ([fst (car exp-pairs)])
            (let ([left-val (value-of (car fst) env)])
              (value-of
                (if (expval->bool left-val)
                  (cadr fst)
                  (cond-exp (cdr exp-pairs))
                )
                env
              )
            )
          )
        )
      )
    )
  )
)

(print
  (expval->list
    (value-of
      (let-exp 'x (const-exp 4)
        (cons-exp
          (var-exp 'x)
          (cons-exp
            (cons-exp
              (diff-exp (var-exp 'x) (const-exp 1))
              (emptylist-exp)
            )
            (emptylist-exp)
          )
        )
      )
      (init-env)
    )
  )
);  (4 (3))
(print
  (expval->list
    (value-of
      (let-exp 'x (const-exp 4)
        (list-exp (list
          (var-exp 'x)
          (list-exp (list
            (diff-exp (var-exp 'x) (const-exp 1))
          ))
        ))
      )
      (init-env)
    )
  )
);  (4 (3))
