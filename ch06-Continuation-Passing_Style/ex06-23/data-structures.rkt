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
    (p-body tf-expression?)
    (env environment?)
  )
  (extend-env-rec*
    (p-names (list-of identifier?))
    (b-varss (list-of (list-of identifier?)))
    (p-bodies (list-of tf-expression?))
    (env environment?)
  )
)

; () -> Env
(define init-env
  (lambda ()
    (empty-env)
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
              (car vals)
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
              (proc-val (procedure (car varss) (car bodies) env))
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
    (vars (list-of identifier?))
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
  (list-val
    (list1 list?)
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

; ExpVal -> Proc
(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (list1) list1)
      (else report-expval-extractor-error 'list val)
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
      (list-val (list1) list1)
      (else report-expval-extractor-error 'any val)
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
      ((list? v) (list-val v))
      (else report-invalid-scheme-value-error v)
    )
  )
)

; () -> Sto
(define empty-store
  (lambda () '())
)

; Sto
(define the-k-store 'uninitialized)

; () -> Sto
(define get-k-store
  (lambda () the-k-store)
)

; () -> Unspecified
(define initialize-k-store!
  (lambda ()
    (set! the-k-store (empty-store))
  )
)

; SimpleExp -> Ref
(define newref
  (lambda (val)
    (let ([next-ref (length the-k-store)])
      (set! the-k-store (append the-k-store (list val)))
      next-ref
    )
  )
)

; Ref -> SimpleExp
(define deref
  (lambda (ref)
    (list-ref the-k-store ref)
  )
)

; Ref * SimpleExp -> ()
(define setref!
  (lambda (ref val)
    (set! the-k-store
      (letrec ([
        setref-inner (lambda (store1 ref1)
            (cond
              ((null? store1) (report-invalid-reference ref the-k-store))
              ((zero? ref1) (cons val (cdr store1)))
              (else (cons (car store1) (setref-inner (cdr store1) (- ref1 1))))
            )
          )
        ])
        (setref-inner the-k-store ref)
      )
    )
  )
)

