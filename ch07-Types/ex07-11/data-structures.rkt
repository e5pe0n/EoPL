#lang eopl

(require "utils.rkt")
(require "lang.rkt")

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
    (saved-val s-val?)
    (saved-env environment?)
  )
  (extend-env*
    (saved-vars (list-of symbol?))
    (saved-vals (list-of s-val?))
    (saved-env environment?)
  )
  (extend-env-rec*
    (p-name (list-of identifier?))
    (b-varss (list-of (list-of identifier?)))
    (p-bodies (list-of expression?))
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
      (empty-env () (report-no-binding-found 'apply-env search-var))
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
      (extend-env-rec* (p-names b-varss p-bodies saved-env)
        (let ([index (list-index (lambda (x) (eqv? x search-var)) p-names)])
          (if index
            (newref
              (proc-val (procedure (list-ref b-varss index) (list-ref p-bodies index) env))
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
    (vars (list-of identifier?))
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
    (a-pair (newref val1) (newref val2))
  )
)

; MutPair -> ExpVal
(define left
  (lambda (p)
    (cases mutpair p
      (a-pair (left-loc right-loc) (deref left-loc))
    )
  )
)

; MutPair -> ExpVal
(define right
  (lambda (p)
    (cases mutpair p
      (a-pair (left-loc right-loc) (deref right-loc))
    )
  )
)

; MutPair * ExpVal -> ExpVal
(define setleft
  (lambda (p val)
    (cases mutpair p
      (a-pair (left-loc right-loc) (setref! left-loc val))
    )
  )
)

; MutPair * ExpVal -> ExpVal
(define setright
  (lambda (p val)
    (cases mutpair p
      (a-pair (left-loc right-loc) (setref! right-loc val))
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
  (mutpair-val
    (mutpair1 mutpair?)
  )
)

; ExpVal -> Number
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else report-expval-extractor-error 'expval->num val)
    )
  )
)

; ExpVal -> Bool
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else report-expval-extractor-error 'expval->bool val)
    )
  )
)

; ExpVal -> Proc
(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc1) proc1)
      (else report-expval-extractor-error 'expval->proc val)
    )
  )
)

; ExpVal -> MutPair
(define expval->mutpair
  (lambda (val)
    (cases expval val
      (mutpair-val (mutpair1) mutpair1)
      (else report-expval-extractor-error 'expval->mutpair val)
    )
  )
)

; String * ExpVal -> ()
(define report-expval-extractor-error
  (lambda (err-name val)
    (eopl:error err-name "Couldn't extract inside value; val=~s" val)
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

; Ref * Sto -> ()
(define report-invalid-reference
  (lambda (ref store)
    (eopl:error 'invalid-ref "invalid reference: ref=~s, store=~s" ref store)
  )
)
