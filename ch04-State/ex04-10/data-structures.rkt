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
  (extend-env-rec
    (p-name identifier?)
    (b-var identifier?)
    (body expression?)
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
      (extend-env-rec (p-name b-var p-body saved-env)
        (if (eqv? p-name search-var)
          (proc-val (procedure b-var p-body env))
          (apply-env saved-env search-var)
        )
      )
    )
  )
)

(define-datatype proc proc?
  (procedure
    (var identifier?)
    (body expression?)
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
  (ref-val
    (num number?)
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

; ExpVal -> Ref
(define expval->ref
  (lambda (val)
    (cases expval val
      (ref-val (num) num)
      (else report-expval-extractor-error 'ref val)
    )
  )
)
; () -> Sto
(define empty-store
  (lambda () '())
)

; Sto
(define the-store 'uninitialized)

; () -> Sto
(define get-store
  (lambda () the-store)
)

; () -> Unspecified
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))
  )
)

; SchemeVal -> Bool
(define reference?
  (lambda (v)
    (integer? v)
  )
)

; ExpVal -> Ref
(define newref
  (lambda (val)
    (let ([next-ref (length the-store)])
      (set! the-store (append the-store (list val)))
      next-ref
    )
  )
)

; Ref -> ExpVal
(define deref
  (lambda (ref)
    (list-ref the-store ref)
  )
)

(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec ([
          setref-inner
          (lambda (store1 ref1)
            (cond
              ((null? store1) (report-invalid-reference ref the-store))
              ((zero? ref) (cons val (cdr store1)))
              (else (cons (car store1) (setref-inner (cdr store1) (- ref 1))))
            )
          )
        ])
        (setref-inner the-store ref)
      )
    )
  )
)