#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "errors.rkt")

(provide (all-defined-out))

; ExpVal = Int + Bool + Proc
; DenVal = Ref(ExpVal)

; SchemeVal -> Bool
(define reference?
  (lambda (v)
    (integer? v)
  )
)

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (saved-var symbol?)
    (saved-val reference?)
    (saved-env environment?)
  )
  (extend-env-rec
    (p-name identifier?)
    (b-vars (list-of identifier?))
    (body expression?)
    (env environment?)
  )
)

; () -> Env
(define init-env
  (lambda ()
    (extend-env 'i 2
      (extend-env 'v 1
        (extend-env 'x 0
          (empty-env)
        )
      )
    )
  )
)

; Env * Var -> Ref
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env () (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? search-var saved-var)
          saved-val ; Ref
          (apply-env saved-env search-var)
        )
      )
      (extend-env-rec (p-name b-vars p-body saved-env)
        (if (eqv? p-name search-var)
          (newref
            (proc-val (procedure b-vars p-body env))
          )
          (apply-env saved-env search-var)
        )
      )
    )
  )
)

(define-datatype proc proc?
  (procedure
    (vars (list-of identifier?))
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

; ExpVal -> List
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
(define the-store 'uninitialized)

; () -> Sto
(define get-store
  (lambda () the-store)
)

; () -> Unspecified
(define initialize-store!
  (lambda ()
    (set! the-store (list (num-val 10) (num-val 5) (num-val 1)))
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

; Ref * ExpVal -> ()
(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec ([
        setref-inner (lambda (store1 ref1)
            (cond
              ((null? store1) (report-invalid-reference ref the-store))
              ((zero? ref1) (cons val (cdr store1)))
              (else (cons (car store1) (setref-inner (cdr store1) (- ref1 1))))
            )
          )
        ])
        (setref-inner the-store ref)
      )
    )
  )
)
