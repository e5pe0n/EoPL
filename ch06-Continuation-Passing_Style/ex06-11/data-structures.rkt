#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "errors.rkt")

(provide (all-defined-out))

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (saved-var symbol?)
    (saved-val s-val?)
    (saved-env environment?)
  )
  (extend-env*
    (saved-vars (list-of symbol?))
    (saved-vals (list-of s-val?))
    (saved-env environment?)
  )
  (extend-env-rec
    (p-name identifier?)
    (b-vars (list-of identifier?))
    (p-body expression?)
    (env environment?)
  )
  (extend-env-rec*
    (p-names (list-of identifier?))
    (b-varss (list-of (list-of identifier?)))
    (p-bodies (list-of expression?))
    (env environment?)
  )
)

; () -> Env
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

; Env * Var -> ExpVal
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
      (extend-env* (saved-vars saved-vals saved-env)
        (let f ([vars saved-vars] [vals saved-vals])
          (if (null? vars)
            (apply-env saved-env search-var)
            (if (eqv? search-var (car vars))
              (car saved-vals)
              (f (cdr vars) (cdr vals))
            )
          )
        )
      )
      (extend-env-rec (p-name b-vars p-body saved-env)
        (if (eqv? p-name search-var)
          (proc-val (procedure b-vars p-body env))
          (apply-env saved-env search-var)
        )
      )
      (extend-env-rec* (p-names b-varss p-bodies saved-env)
        (let f ([names p-names] [varss b-varss] [bodies p-bodies])
          (if (null? names)
            (apply-env saved-env search-var)
            (if (eqv? search-var (car names))
              (proc-val (procedure (car names) (car varss) (car bodies) env))
              (f (cdr names) (cdr varss) (cdr bodies))
            )
          )
        )
      )
    )
  )
)

(define-datatype proc proc?
  (procedure
    (var identifier?)
    (body tf-expression?)
    (saved-env environment?)
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

; ExpVal -> Number
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else report-expval-extractor-error 'num val)
    )
  )
)

; ExpVal -> Bool
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else report-expval-extractor-error 'bool val)
    )
  )
)

; ExpVal -> Proc
(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc1) proc1)
      (else report-expval-extractor-error 'proc val)
    )
  )
)

; ExpVal -> SchemeVal
(define expval->any
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (bool-val (bool) bool)
      (proc-val (proc1) proc1)
      (else report-expval-extractor-error 'list val)
    )
  )
)

; SchemeVal -> ExpVal
(define any->expval
  (lambda (v)
    (cond
      ((number? v) (num-val v))
      ((boolean? v) (bool-val v))
      ((proc? v) (proc-val v))
      (else report-invalid-scheme-value-error v)
    )
  )
)

