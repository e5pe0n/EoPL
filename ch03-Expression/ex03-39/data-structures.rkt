#lang eopl

(require "utils.rkt")
(require "errors.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; Senv = Listof(Sym)
; Lexaddr = N

; () -> Senv
(define empty-senv
  (lambda () '())
)

; Var * Senv -> Senv
(define extend-senv
  (lambda (var senv)
    (cons var senv)
  )
)

; Senv * Var -> Lexaddr
; Translate variable name to lexical address
(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv) (report-unbound-var var))
      ((eqv? var (car senv)) 0)
      (else (+ 1 (apply-senv (cdr senv) var)))
    )
  )
)


; SchemeVal -> Bool
(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)
  )
)

; () -> Nameless-env
(define empty-nameless-env
  (lambda () '())
)

; ExpVal * Nameless-env -> Nameless-env
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)
  )
)

; Nameless-env * Lexaddr -> Expval
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)
  )
)

; Proc = ExpVal -> ExpVal

(define-datatype proc proc?
  (procedure
    (body expression?)
    (saved-nameless-env nameless-environment?)
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

; ExpVal -> List
(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (list1) list1)
      (else report-expval-extractor-error 'list val)
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
      (list-val (list1) list1)
      (else report-expval-extractor-error 'any val)
    )
  )
)

; SchemeVal -> ExpVal
(define any->expval
  (lambda (x)
    (cond
      ((number? x) (num-val x))
      ((boolean? x) (bool-val x))
      ((list? x) (list-val x))
      (else report-invalid-scheme-val x)
    )
  )
)