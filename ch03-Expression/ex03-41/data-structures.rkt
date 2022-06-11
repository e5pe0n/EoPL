#lang eopl

(require "utils.rkt")
(require "errors.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; Senv = Listof(Lisof(Sym), Bool)
; Lexaddr = (N, N)

; () -> Senv
(define empty-senv
  (lambda () '())
)

; Listof(Sym) * Bool * Senv -> Senv
(define extend-senv
  (lambda (vars letrec-bound senv)
    (cons (cons vars letrec-bound) senv)
  )
)

; Senv * Var -> (Lexaddr, Bool)
; Translate variable name to lexical address
(define apply-senv
  (lambda (senv var)
    (if (null? senv)
      (report-unbound-var var)
      (let ([x (car senv)])
        (if (memv var (car x))
          (cons
            (cons 0 (list-index (car x) var))
            (cdr x)
          )
          (let ([res (apply-senv (cdr senv)  var)])
            (cons (cons (+ 1 (caar res)) (cdar res)) (cdr res))
          )
        )
      )
    )
  )
)


; SchemeVal -> Bool
(define nameless-environment?
  (lambda (x)
    ((list-of (list-of expval?)) x)
  )
)

; () -> Nameless-env
(define empty-nameless-env
  (lambda () '())
)

; Nameless-env = Listof(Listof(ExpVal))
; ExpVal * Nameless-env -> Nameless-env
(define extend-nameless-env
  (lambda (vals nameless-env)
    (cons vals nameless-env)
  )
)

; Exp * Nameless-env -> Nameless-env
(define extend-nameless-env-rec
  (lambda (p-bodys nameless-env)
    (cons (map (lambda (x) (proc-val (procedure x nameless-env))) p-bodys) nameless-env)
  )
)

; Nameless-env * Lexaddr -> Expval
(define apply-nameless-env
  (lambda (nameless-env addr)
    (list-ref
      (list-ref nameless-env (car addr))
      (cdr addr)
    )
  )
)

; Nameless-env * LExaddr -> ExpVal
(define apply-nameless-env-rec
  (lambda (nameless-env addr)
    (let ([res (list-ref nameless-env (car addr))])
      (let ([bodys (map (lambda (x) (cases proc (expval->proc x) (procedure (b sne) b))) res)])
        (proc-val
          (procedure (list-ref bodys (cdr addr)) (extend-nameless-env-rec bodys nameless-env))
        )
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