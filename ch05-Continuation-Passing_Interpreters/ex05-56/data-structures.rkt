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
    (th-id integer?)
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
      (extend-env-rec (p-name b-vars p-body saved-env th-id)
        (if (eqv? p-name search-var)
          (newref
            (proc-val (procedure b-vars p-body env))
            th-id
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


(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont (cont continuation?))
  (let-exp-cont
    (vars (list-of identifier?))
    (exps (list-of expression?))
    (rvars (list-of identifier?))
    (vals (list-of expval?))
    (body expression?)
    (env environment?)
    (cont continuation?)
  )
  (if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (env environment?)
    (cont continuation?)
  )
  (binary-op1-cont
    (op procedure?)
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (binary-op2-cont
    (op procedure?)
    (val1 expval?)
    (saved-cont continuation?)
  )
  (rator-cont
    (rands (list-of expression?))
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (rand1-cont
    (rands (list-of expression?))
    (proc1 proc?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (rand2-cont
    (proc1 proc?)
    (saved-cont continuation?)
  )
  (cons1-cont
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (cons2-cont
    (val1 expval?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (null?-cont
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (car-cont
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (cdr-cont
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (list1-cont
    (exps (list-of expression?))
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (list2-cont
    (val1 expval?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (set-rhs-cont
    (saved-env environment?)
    (var identifier?)
    (saved-cont continuation?)
  )
  (begin-cont
    (exps (list-of expression?))
    (env environment?)
    (saved-cont continuation?)
  )
  (print-cont
    (saved-cont continuation?)
  )
  (spawn-cont
    (saved-cont continuation?)
  )
  (end-main-thread-cont)
  (end-subthread-cont)
  (kill-cont
    (saved-cont continuation?)
  )
  (send1-cont
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?)
  )
  (send2-cont
    (val1 expval?)
    (saved-cont continuation?)
  )
)

(define-datatype thread thread?
  (a-thread
    (proc1 procedure?)
    (th-id integer?)
    (parent-th-id integer?)
  )
)

(define the-stores '())

; () -> Sto
(define empty-store
  (lambda () '())
)

(define new-store
  (lambda ()
    (set! the-stores (append the-stores (list (empty-store))))
  )
)

(define get-store
  (lambda (th-id)
    (list-ref the-stores th-id)
  )
)

(define set-store!
  (lambda (store th-id)
    (set! the-stores
      (let f ([i th-id] [stores the-stores])
        (if (zero? i)
          (cons store (cdr stores))
          (cons (car stores) (f (- i 1) (cdr stores)))
        )
      )
    )
  )
)

; ExpVal * Int -> Ref
(define newref
  (lambda (val th-id)
    (let* ([store (get-store th-id)] [next-ref (length store)])
      (set-store! (append store (list val)) th-id)
      next-ref
    )
  )
)

; Ref * Int -> ExpVal
(define deref
  (lambda (ref th-id)
    (list-ref (get-store th-id) ref)
  )
)

; Ref * ExpVal * Int -> ()
(define setref!
  (lambda (ref val th-id)
    (set-store!
      (let f ([ref1 ref] [store (get-store th-id)])
        (cond
          ((null? store) (report-invalid-reference ref (get-store th-id)))
          ((zero? ref1) (cons val (cdr store)))
          (else (cons (car store) (f (- ref1 1) (cdr store))))
        )
      )
      th-id
    )
  )
)
