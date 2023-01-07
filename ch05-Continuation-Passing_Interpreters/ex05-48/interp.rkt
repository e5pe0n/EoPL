#lang eopl

(require "utils.rkt")
(require "data-structures.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; FinalAnswer = ExpVal
; Thread = (a-thread (() -> ExpVal))
(define-datatype thread thread?
  (a-thread (proc1 procedure?))
)

(define run-thread
  (lambda (th)
    (cases thread th
      (a-thread (proc1)
        (proc1)
      )
    )
  )
)

(define the-ready-queue 'uninitialized)
(define the-final-answer 'uninitialized)
(define the-max-time-slice 'uninitialized)
(define the-time-remaining 'uninitialized)


(define empty-queue
  (lambda () '())
)

(define empty?
  (lambda (q) (null? q))
)

; Listof(Thread) * Thread -> Listof(Thread)
(define enqueue
  (lambda (q th) (append q (list th)))
)

; Listof(Thread) * f(Thread, Listof(Thread)) -> ExpVal
(define dequeue
  (lambda (q f) (f (car q) (cdr q)))
)

; Int -> Unspecified
(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice)
  )
)

; Thread -> Unspecified
(define place-on-ready-queue!
  (lambda (th)
    (set! the-ready-queue (enqueue the-ready-queue th))
  )
)

; () -> FinalAnswer
(define run-next-thread
  (lambda ()
    (if (empty? the-ready-queue)
      the-final-answer
      (dequeue the-ready-queue
        (lambda (first-ready-thread other-ready-threads)
          (set! the-ready-queue other-ready-threads)
          (set! the-time-remaining the-max-time-slice)
          (run-thread first-ready-thread)
        )
      )
    )
  )
)

; ExpVal -> Unspecified
(define set-final-answer!
  (lambda (val)
    (set! the-final-answer val)
  )
)

; () -> Bool
(define time-expired?
  (lambda ()
    (zero? the-time-remaining)
  )
)

; () -> Unspecified
(define decrement-timer!
  (lambda ()
    (set! the-time-remaining (- the-time-remaining 1))
  )
)

; Mutex * Thread -> FinalAnswer
(define wait-for-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
        (cond
          ((deref ref-to-closed?)
            (setref! ref-to-wait-queue (enqueue (deref ref-to-wait-queue) th))
            (run-next-thread)
          )
          (else
            (setref! ref-to-closed? #t)
            (run-thread th)
          )
        )
      )
    )
  )
)

; Mutex * Thread -> FinalAnswer
(define signal-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
        (let ([closed? (deref ref-to-closed?)] [wait-queue (deref ref-to-wait-queue)])
          (if closed?
            (if (empty? wait-queue)
              (setref! ref-to-closed? #f)
              (dequeue wait-queue
                (lambda (first-waiting-th other-waiting-ths)
                  (place-on-ready-queue! first-waiting-th)
                  (setref! ref-to-wait-queue other-waiting-ths)
                )
              )
            )
            (num-val 54)
          )
          (run-thread th)
        )
      )
    )
  )
)


; Int * Program -> FinalAnswer
(define value-of-program
  (lambda (timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      (a-program (exp1)
        (value-of/k exp1 (init-env) (end-main-thread-cont))
      )
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
  (wait-cont
    (saved-cont continuation?)
  )
  (signal-cont
    (saved-cont continuation?)
  )
)


; Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (if (time-expired?)
      (begin
        (place-on-ready-queue! (a-thread (lambda () (apply-cont cont val))))
        (run-next-thread)
      )
      (begin
        (decrement-timer!)
        (cases continuation cont
          (end-cont ()
            (begin
              (eopl:printf "End of Computation.~%")
              val
            )
          )
          (zero1-cont (saved-cont)
            (apply-cont saved-cont
              (bool-val (zero? (expval->num val)))
            )
          )
          (let-exp-cont (vars exps rvars vals body saved-env saved-cont)
            (if (null? exps)
              (value-of/k body
                (let f ([vrs rvars] [vls (cons val vals)])
                  (if (null? vrs)
                    saved-env
                    (extend-env (car vrs) (newref (car vls)) (f (cdr vrs) (cdr vls)))
                  )
                )
                saved-cont
              )
              (value-of/k (car exps)
                saved-env
                (let-exp-cont (cdr vars) (cdr exps) (cons (car vars) rvars) (cons val vals) body saved-env saved-cont)
              )
            )
          )
          (if-test-cont (exp2 exp3 saved-env saved-cont)
            (if (expval->bool val)
              (value-of/k exp2 saved-env saved-cont)
              (value-of/k exp3 saved-env saved-cont)
            )
          )
          (binary-op1-cont (op exp2 saved-env saved-cont)
            (value-of/k exp2 saved-env
              (binary-op2-cont op val saved-cont)
            )
          )
          (binary-op2-cont (op val1 saved-cont)
            (let ([num1 (expval->num val1)] [num2 (expval->num val)])
              (apply-cont saved-cont
                (num-val (op num1 num2))
              )
            )
          )
          (rator-cont (rands saved-env saved-cont)
            (let ([proc1 (expval->proc val)])
              (if (null? rands)
                (apply-procedure/k proc1 '() saved-cont)
                (value-of/k (car rands) saved-env
                  (let ([rds (cdr rands)])
                    (if (null? rds)
                      (rand2-cont proc1 saved-cont)
                      (rand1-cont rds proc1 saved-env saved-cont)
                    )
                  )
                )
              )
            )
          )
          (rand1-cont (rands proc1 saved-env saved-cont)
            (value-of/k (car rands) saved-env
              (cases proc proc1
                (procedure (vars body p-saved-env)
                  (let (
                      [rds (cdr rands)]
                      [proc2 (procedure (cdr vars) body (extend-env (car vars) (newref val) p-saved-env))]
                    )
                    (if (null? rds)
                      (rand2-cont proc2 saved-cont)
                      (rand1-cont rds proc2 saved-env saved-cont)
                    )
                  )
                )
              )
            )
          )
          (rand2-cont (proc1 saved-cont)
            (apply-procedure/k proc1 (list val) saved-cont)
          )
          (cons1-cont (exp2 saved-env saved-cont)
            (value-of/k exp2 saved-env
              (cons2-cont val saved-env saved-cont)
            )
          )
          (cons2-cont (val1 saved-env saved-cont)
            (let ([any1 (expval->any val1)] [any2 (expval->any val)])
              (apply-cont saved-cont
                (list-val (cons any1 any2))
              )
            )
          )
          (null?-cont (saved-env saved-cont)
            (apply-cont saved-cont
              (bool-val (null? (expval->list val)))
            )
          )
          (car-cont (saved-env saved-cont)
            (let ([list1 (expval->list val)])
              (apply-cont saved-cont
                (any->expval (car list1))
              )
            )
          )
          (cdr-cont (saved-env saved-cont)
            (let ([list1 (expval->list val)])
              (apply-cont saved-cont
                (any->expval (cdr list1))
              )
            )
          )
          (list1-cont (exps saved-env saved-cont)
            (value-of/k (list-exp exps) saved-env
              (list2-cont val saved-env saved-cont)
            )
          )
          (list2-cont (val1 saved-env saved-cont)
            (apply-cont saved-cont (list-val (cons (expval->any val1) (expval->list val)))
            )
          )
          (set-rhs-cont (saved-env var saved-cont)
            (begin
              (setref! (apply-env saved-env var) val)
              (apply-cont saved-cont (num-val 27))
            )
          )
          (begin-cont (exps saved-env saved-cont)
            (if (null? exps)
              (apply-cont saved-cont val)
              (value-of/k (car exps) saved-env
                (begin-cont (cdr exps) saved-env saved-cont)
              )
            )
          )
          (print-cont (saved-cont)
            (begin
              (println val)
              (apply-cont saved-cont (num-val 26))
            )
          )
          (spawn-cont (saved-cont)
            (let ([proc1 (expval->proc val)])
              (place-on-ready-queue!
                (a-thread
                  (lambda ()
                    (apply-procedure/k proc1
                      '(num-val 28)
                      (end-subthread-cont)
                    )
                  )
                )
              )
              (apply-cont saved-cont (num-val 73))
            )
          )
          (end-main-thread-cont ()
            (set-final-answer! val)
            (run-next-thread)
          )
          (end-subthread-cont ()
            (run-next-thread)
          )
          (wait-cont (saved-cont)
            (wait-for-mutex
              (expval->mutex val)
              (a-thread (lambda () (apply-cont saved-cont (num-val 52))))
            )
          )
          (signal-cont (saved-cont)
            (signal-mutex
              (expval->mutex val)
              (a-thread (lambda () (apply-cont saved-cont (num-val 53))))
            )
          )
        )
      )
    )
  )
)

; Exp x Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (deref (apply-env env var))))
      (proc-exp (vars body)
        (apply-cont cont
          (proc-val (procedure vars body env))
        )
      )
      (letrec-exp (p-name b-vars p-body letrec-body)
        (value-of/k
          letrec-body
          (extend-env-rec p-name b-vars p-body env)
          cont
        )
      )
      (zero?-exp (exp1)
        (value-of/k exp1 env (zero1-cont cont))
      )
      (let-exp (vars exps body)
        (if (null? vars)
          (value-of/k body env cont)
          (value-of/k (car exps) env
            (let-exp-cont (cdr vars) (cdr exps) (list (car vars)) '() body env cont)
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (if-test-cont exp2 exp3 env cont)
        )
      )
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (binary-op1-cont - exp2 env cont)
        )
      )
      (mul-exp (exp1 exp2)
        (value-of/k exp1 env
          (binary-op1-cont * exp2 env cont)
        )
      )
      (call-exp (rator rands)
        (value-of/k rator env
          (rator-cont rands env cont)
        )
      )
      (emptylist-exp () (apply-cont cont (list-val '())))
      (cons-exp (exp1 exp2)
        (value-of/k exp1 env
          (cons1-cont exp2 env cont)
        )
      )
      (null?-exp (exp1)
        (value-of/k exp1 env
          (null?-cont env cont)
        )
      )
      (car-exp (exp1)
        (value-of/k exp1 env
          (car-cont env cont)
        )
      )
      (cdr-exp (exp1)
        (value-of/k exp1 env
          (cdr-cont env cont)
        )
      )
      (list-exp (exps)
        (if (null? exps)
          (apply-cont cont (list-val '()))
          (value-of/k (car exps) env
            (list1-cont (cdr exps) env cont)
          )
        )
      )
      (assign-exp (var exp1)
        (value-of/k exp1 env
          (set-rhs-cont env var cont)
        )
      )
      (begin-exp (exp1 exps)
        (value-of/k exp1 env
          (begin-cont exps env cont)
        )
      )
      (print-exp (exp1)
        (value-of/k exp1 env (print-cont cont))
      )
      (spawn-exp (exp1)
        (value-of/k exp1 env (spawn-cont cont))
      )
      (mutex-exp ()
        (apply-cont cont (mutex-val (new-mutex)))
      )
      (wait-exp (exp1)
        (value-of/k exp1 env (wait-cont cont))
      )
      (signal-exp (exp1)
        (value-of/k exp1 env (signal-cont cont))
      )
    )
  )
)


; Proc * Listof(ExpVal) * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 vals cont)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of/k body
          (if (null? vars)
            saved-env
            (extend-env (car vars) (newref (car vals)) saved-env)
          )
          cont
        )
      )
    )
  )
)
