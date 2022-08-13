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
    (p-names (list-of identifier?))
    (b-vars (list-of identifier?))
    (p-bodies (list-of expression?))
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
          saved-val
          (apply-env saved-env search-var)
        )
      )
      (extend-env-rec (p-names b-vars p-bodies saved-env)
        (let ([n (location search-var p-names)])
          (if n
            (newref
              (proc-val
                (procedure
                  (list-ref b-vars n)
                  (list-ref p-bodies n)
                  env
                )
              )
            )
            (apply-env saved-env search-var)
          )
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

(define-datatype mutpair mutpair?
  (a-pair
    (left-loc reference?)
    (right-loc reference?)
  )
)

; ExpVal * ExpVal -> MutPair
(define make-pair
  (lambda (val1 val2)
    (a-pair
      (newref val1)
      (newref val2)
    )
  )
)

; MutPair -> ExpVal
(define left
  (lambda (p)
    (cases mutpair p
      (a-pair (left-loc right-loc)
        (deref left-loc)
      )
    )
  )
)

; MutPair -> ExpVal
(define right
  (lambda (p)
    (cases mutpair p
      (a-pair (left-loc right-loc)
        (deref right-loc)
      )
    )
  )
)

; MutPair * ExpVal -> Unspecified
(define setleft
  (lambda (p val)
    (cases mutpair p
      (a-pair (left-loc right-loc)
        (setref! left-loc val)
      )
    )
  )
)

; MutPair * ExpVal -> Unspecified
(define setright
  (lambda (p val)
    (cases mutpair p
      (a-pair (left-loc right-loc)
        (setref! right-loc val)
      )
    )
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
  (mutpair-val
    (mutpair mutpair?)
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

; ExpVal -> MutPair
(define expval->mutpair
  (lambda (val)
    (cases expval val
      (mutpair-val (mutpair) mutpair)
      (else report-expval-extractor-error 'mutpair val)
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


