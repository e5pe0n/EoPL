#lang eopl

(require "utils.rkt")
(require "data-structures.rkt")
(require "lang.rkt")

(provide (all-defined-out))

(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define proc1 'uninitialized)
(define th 'uninitialized)

(define th-cont 'uninitialized)
(define th-proc 'uninitialized)

; FinalAnswer = ExpVal
; Thread = () -> ExpVal)

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

; () -> Unspecified
; th: Thread
(define place-on-ready-queue!
  (lambda ()
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
          (set! th first-ready-thread)
          (th)
        )
      )
    )
  )
)

; () -> Unspecified
; val: ExpVal
(define set-final-answer!
  (lambda ()
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

; () -> FinalAnswer
; val: ExpVal
; th: Thread
(define wait-for-mutex
  (lambda ()
    (cases mutex (expval->mutex val)
      (a-mutex (ref-to-closed? ref-to-wait-queue)
        (cond
          ((deref ref-to-closed?)
            (setref! ref-to-wait-queue (enqueue (deref ref-to-wait-queue) th))
            (run-next-thread)
          )
          (else
            (setref! ref-to-closed? #t)
            (th)
          )
        )
      )
    )
  )
)

; () -> FinalAnswer
; val: ExpVal
; th: Thread
(define signal-mutex
  (lambda ()
    (cases mutex (expval->mutex val)
      (a-mutex (ref-to-closed? ref-to-wait-queue)
        (let ([closed? (deref ref-to-closed?)] [wait-queue (deref ref-to-wait-queue)])
          (if closed?
            (if (empty? wait-queue)
              (setref! ref-to-closed? #f)
              (dequeue wait-queue
                (lambda (first-waiting-th other-waiting-ths)
                  (set! th first-waiting-th)
                  (place-on-ready-queue!)
                  (setref! ref-to-wait-queue other-waiting-ths)
                )
              )
            )
            (num-val 54)
          )
          (th)
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
        (set! cont (end-main-thread-cont))
        (set! exp exp1)
        (set! env (init-env))
        (set! th-cont
          (lambda (saved-cont saved-val)
            (lambda ()
              (set! cont saved-cont)
              (set! val saved-val)
              (apply-cont)
            )
          )
        )
        (set! th-proc
          (lambda (saved-proc saved-val saved-cont)
            (lambda ()
              (set! cont saved-cont)
              (set! proc1 saved-proc)
              (set! val saved-val)
              (apply-procedure/k)
            )
          )
        )
        (value-of/k)
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

; () -> FinalAnswer
; cont: Cont
; expval: ExpVal
(define apply-cont
  (lambda ()
    (if (time-expired?)
      (begin
        (set! th (th-cont cont val))
        (place-on-ready-queue!)
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
            (set! cont saved-cont)
            (set! val (bool-val (zero? (expval->num val))))
            (apply-cont)
          )
          (let-exp-cont (vars exps rvars vals body saved-env saved-cont)
            (if (null? exps)
              (begin
                (set! cont saved-cont)
                (set! exp body)
                (set! env
                  (let f ([vrs rvars] [vls (cons val vals)])
                    (if (null? vrs)
                      saved-env
                      (extend-env (car vrs) (newref (car vls)) (f (cdr vrs) (cdr vls)))
                    )
                  )
                )
                (value-of/k)
              )
              (begin
                (set! cont
                  (let-exp-cont (cdr vars) (cdr exps) (cons (car vars) rvars) (cons val vals) body saved-env saved-cont)
                )
                (set! exp (car exps))
                (set! env saved-env)
                (value-of/k)
              )
            )
          )
          (if-test-cont (exp2 exp3 saved-env saved-cont)
            (set! cont saved-cont)
            (set! exp
              (if (expval->bool val)
                exp2
                exp3
              )
            )
            (set! env saved-env)
            (value-of/k)
          )
          (binary-op1-cont (op exp2 saved-env saved-cont)
            (set! cont (binary-op2-cont op val saved-cont))
            (set! exp exp2)
            (set! env saved-env)
            (value-of/k)
          )
          (binary-op2-cont (op val1 saved-cont)
            (let ([num1 (expval->num val1)] [num2 (expval->num val)])
              (set! cont saved-cont)
              (set! val (num-val (op num1 num2)))
              (apply-cont)
            )
          )
          (rator-cont (rands saved-env saved-cont)
            (let ([rator-proc (expval->proc val)])
              (if (null? rands)
                (begin
                  (set! cont saved-cont)
                  (set! proc1 rator-proc)
                  (apply-procedure/k)
                )
                (begin
                  (set! cont
                    (let ([rds (cdr rands)])
                      (if (null? rds)
                        (rand2-cont rator-proc saved-cont)
                        (rand1-cont rds rator-proc saved-env saved-cont)
                      )
                    )
                  )
                  (set! exp (car rands))
                  (set! env saved-env)
                  (value-of/k)
                )
              )
            )
          )
          (rand1-cont (rands rator-proc saved-env saved-cont)
            (set! cont
              (cases proc rator-proc
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
            (set! exp (car rands))
            (set! env saved-env)
            (value-of/k)
          )
          (rand2-cont (rator-proc saved-cont)
            (set! cont saved-cont)
            (set! proc1 rator-proc)
            (apply-procedure/k)
          )
          (cons1-cont (exp2 saved-env saved-cont)
            (set! cont (cons2-cont val saved-env saved-cont))
            (set! exp exp2)
            (set! env saved-env)
            (value-of/k)
          )
          (cons2-cont (val1 saved-env saved-cont)
            (set! cont saved-cont)
            (set! val
              (let ([any1 (expval->any val1)] [any2 (expval->any val)])
                (list-val (cons any1 any2))
              )
            )
            (apply-cont)
          )
          (null?-cont (saved-env saved-cont)
            (set! cont saved-cont)
            (set! val (bool-val (null? (expval->list val))))
            (apply-cont)
          )
          (car-cont (saved-env saved-cont)
            (set! cont saved-cont)
            (set! val
              (let ([list1 (expval->list val)])
                (any->expval (car list1))
              )
            )
            (apply-cont)
          )
          (cdr-cont (saved-env saved-cont)
            (set! cont saved-cont)
            (set! val
              (let ([list1 (expval->list val)])
                (any->expval (cdr list1))
              )
            )
            (apply-cont)
          )
          (list1-cont (exps saved-env saved-cont)
            (set! cont (list2-cont val saved-env saved-cont))
            (set! exp (list-exp exps))
            (set! env saved-env)
            (value-of/k)
          )
          (list2-cont (val1 saved-env saved-cont)
            (set! cont saved-cont)
            (set! val (list-val (cons (expval->any val1) (expval->list val))))
            (apply-cont)
          )
          (set-rhs-cont (saved-env var saved-cont)
            (begin
              (setref! (apply-env saved-env var) val)
              (set! cont saved-cont)
              (set! val (num-val 27))
              (apply-cont)
            )
          )
          (begin-cont (exps saved-env saved-cont)
            (if (null? exps)
              (begin
                (set! cont saved-cont)
                (set! val val)
                (apply-cont)
              )
              (begin
                (set! cont (begin-cont (cdr exps) saved-env saved-cont))
                (set! exp (car exps))
                (set! env saved-env)
                (value-of/k)
              )
            )
          )
          (print-cont (saved-cont)
            (begin
              (println val)
              (set! cont saved-cont)
              (set! val (num-val 26))
              (apply-cont)
            )
          )
          (spawn-cont (saved-cont)
            (let ([saved-proc (expval->proc val)])
              (set! th (th-proc saved-proc (num-val 28) (end-subthread-cont)))
              (place-on-ready-queue!)
              (set! cont saved-cont)
              (set! val (num-val 73))
              (apply-cont)
            )
          )
          (end-main-thread-cont ()
            (set-final-answer!)
            (run-next-thread)
          )
          (end-subthread-cont ()
            (run-next-thread)
          )
          (wait-cont (saved-cont)
            (set! th (th-cont saved-cont (num-val 52)))
            (wait-for-mutex)
          )
          (signal-cont (saved-cont)
            (set! th (th-cont saved-cont (num-val 53)))
            (signal-mutex)
          )
        )
      )
    )
  )
)

; () -> FinalAnswer
; exp: Exp
; env: Env
; cont: Cont
(define value-of/k
  (lambda ()
    (cases expression exp
      (const-exp (num)
        (set! val (num-val num))
        (apply-cont)
      )
      (var-exp (var)
        (set! val (deref (apply-env env var)))
        (apply-cont)
      )
      (proc-exp (vars body)
        (set! val (proc-val (procedure vars body env)))
        (apply-cont)
      )
      (letrec-exp (p-name b-vars p-body letrec-body)
        (set! exp letrec-body)
        (set! env (extend-env-rec p-name b-vars p-body env))
        (value-of/k)
      )
      (zero?-exp (exp1)
        (set! cont (zero1-cont cont))
        (set! exp exp1)
        (value-of/k)
      )
      (let-exp (vars exps body)
        (if (null? vars)
          (begin
            (set! exp body)
            (value-of/k)
          )
          (begin
            (set! cont
              (let-exp-cont (cdr vars) (cdr exps) (list (car vars)) '() body env cont)
            )
            (set! exp (car exps))
            (value-of/k)
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (set! cont (if-test-cont exp2 exp3 env cont))
        (set! exp exp1)
        (value-of/k)
      )
      (diff-exp (exp1 exp2)
        (set! cont (binary-op1-cont - exp2 env cont))
        (set! exp exp1)
        (value-of/k)
      )
      (mul-exp (exp1 exp2)
        (set! cont (binary-op1-cont * exp2 env cont))
        (set! exp exp1)
        (value-of/k)
      )
      (call-exp (rator rands)
        (set! cont (rator-cont rands env cont))
        (set! exp rator)
        (value-of/k)
      )
      (emptylist-exp ()
        (set! val (list-val '()))
        (apply-cont)
      )
      (cons-exp (exp1 exp2)
        (set! cont (cons1-cont exp2 env cont))
        (set! exp exp1)
        (value-of/k)
      )
      (null?-exp (exp1)
        (set! cont (null?-cont env cont))
        (set! exp exp1)
        (value-of/k)
      )
      (car-exp (exp1)
        (set! cont (car-cont env cont))
        (set! exp exp1)
        (value-of/k)
      )
      (cdr-exp (exp1)
        (set! cont (cdr-cont env cont))
        (set! exp exp1)
        (value-of/k)
      )
      (list-exp (exps)
        (if (null? exps)
          (begin
            (set! val (list-val '()))
            (apply-cont)
          )
          (begin
            (set! cont (list1-cont (cdr exps) env cont))
            (set! exp (car exps))
            (value-of/k)
          )
        )
      )
      (assign-exp (var exp1)
        (set! cont (set-rhs-cont env var cont))
        (set! exp exp1)
        (value-of/k)
      )
      (begin-exp (exp1 exps)
        (set! cont (begin-cont exps env cont))
        (set! exp exp1)
        (value-of/k)
      )
      (print-exp (exp1)
        (set! cont (print-cont cont))
        (set! exp exp1)
        (value-of/k)
      )
      (spawn-exp (exp1)
        (set! cont (spawn-cont cont))
        (set! exp exp1)
        (value-of/k)
      )
      (mutex-exp ()
        (set! val (mutex-val (new-mutex)))
        (apply-cont)
      )
      (wait-exp (exp1)
        (set! cont (wait-cont cont))
        (set! exp exp1)
        (value-of/k)
      )
      (signal-exp (exp1)
        (set! cont (signal-cont cont))
        (set! exp exp1)
        (value-of/k)
      )
    )
  )
)


; () -> FinalAnswer
; proc1: Proc
; val: ExpVal
; cont: Cont
(define apply-procedure/k
  (lambda ()
    (cases proc proc1
      (procedure (vars body saved-env)
        (set! exp body)
        (set! env
          (if (null? vars)
            saved-env
            (extend-env (car vars) (newref val) saved-env)
          )
        )
        (value-of/k)
      )
    )
  )
)
