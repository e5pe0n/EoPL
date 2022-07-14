#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "errors.rkt")

(provide (all-defined-out))

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (saved-vars (list-of symbol?))
    (saved-val (list-of s-val?))
    (saved-env environment?)
  )
  (extend-env-rec
    (p-names (list-of identifier?))
    (b-vars (list-of (list-of identifier?)))
    (p-bodies (list-of expression?))
    (env environment?)
  )
)

; () -> Env
(define init-env
  (lambda ()
    (extend-env (list 'i 'v 'x) (list 2 1 0) (empty-env))
  )
)

; Env * Var -> Ref
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env () (report-no-binding-found search-var))
      (extend-env (saved-vars saved-vals saved-env)
        (let ([n (location search-var saved-vars)])
          (if n
            (let ([val (list-ref saved-vals n)])
              (if (vector? val)
                (vector-ref val 0)
                val
              )
            )
            (apply-env saved-env search-var)
          )
        )
      )
      (extend-env-rec (p-names b-vars-list p-bodies saved-env)
        (let ([vecs (map (lambda (x) (make-vector 1)) p-names)])
          (let ([new-env (extend-env p-names vecs saved-env)])
            (map
              (lambda (vec b-vars p-body)
                (vector-set! vec 0
                  (newref
                    (proc-val (procedure b-vars p-body new-env))
                  )
                )
              )
              vecs b-vars-list p-bodies
            )
            (apply-env new-env search-var)
          )
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
    (set! the-store (list (num-val 10) (num-val 5) (num-val 1)))
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

; Ref * ExpVal -> ()
(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec ([
          setref-inner
          (lambda (store1 ref1)
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
