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

; Var * Bool * Senv -> Senv
(define extend-senv
  (lambda (var letrec-bound senv)
    (cons (cons var letrec-bound) senv)
  )
)

; Senv * Var -> (Lexaddr, Bool)
; Translate variable name to lexical address
(define apply-senv
  (lambda (senv var)
    (let ([head (car senv)])
      (cond
        ((null? senv) (report-unbound-var var))
        ((eqv? var (car head)) (cons 0 (cdr head)))
        (else
          (let ([res (apply-senv (cdr senv) var)])
            (cons (+ 1 (car res)) (cdr res))
          )
        )
      )
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

; Exp * Nameless-env -> Nameless-env
(define extend-nameless-env-rec
  (lambda (p-body nameless-env)
    (cons (proc-val (procedure p-body nameless-env)) nameless-env)
  )
)

; Nameless-env * Lexaddr -> Expval
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)
  )
)

; Nameless-env * LExaddr -> ExpVal
(define apply-nameless-env-rec
  (lambda (nameless-env n)
    (cases proc (expval->proc (list-ref nameless-env n))
      (procedure (body saved-nameless-env)
        (proc-val (procedure body (extend-nameless-env-rec body nameless-env)))
      )
    )
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