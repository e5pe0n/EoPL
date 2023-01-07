#lang eopl

(require "utils.rkt")
(require "data-structures.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; FinalAnswer = ExpVal
; Thread = () -> ExpVal
; Cont = Listof(Frame)

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
          (first-ready-thread)
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
            (th)
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
        (value-of/k exp1 (init-env) (list (end-main-thread-frame)))
      )
    )
  )
)


(define-datatype frame frame?
  (zero1-frame)
  (let-exp-frame
    (vars (list-of identifier?))
    (exps (list-of expression?))
    (rvars (list-of identifier?))
    (vals (list-of expval?))
    (body expression?)
    (env environment?)
  )
  (if-test-frame
    (exp2 expression?)
    (exp3 expression?)
    (env environment?)
  )
  (binary-op1-frame
    (op procedure?)
    (exp2 expression?)
    (saved-env environment?)
  )
  (binary-op2-frame
    (op procedure?)
    (val1 expval?)
  )
  (rator-frame
    (rands (list-of expression?))
    (saved-env environment?)
  )
  (rand1-frame
    (rands (list-of expression?))
    (proc1 proc?)
    (saved-env environment?)
  )
  (rand2-frame
    (proc1 proc?)
  )
  (cons1-frame
    (exp2 expression?)
    (saved-env environment?)
  )
  (cons2-frame
    (val1 expval?)
    (saved-env environment?)
  )
  (null?-frame
    (saved-env environment?)
  )
  (car-frame
    (saved-env environment?)
  )
  (cdr-frame
    (saved-env environment?)
  )
  (list1-frame
    (exps (list-of expression?))
    (saved-env environment?)
  )
  (list2-frame
    (val1 expval?)
    (saved-env environment?)
  )
  (set-rhs-frame
    (saved-env environment?)
    (var identifier?)
  )
  (begin-frame
    (exps (list-of expression?))
    (env environment?)
  )
  (print-frame)
  (spawn-frame)
  (end-main-thread-frame)
  (end-subthread-frame)
  (wait-frame)
  (signal-frame)
)


; Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (if (time-expired?)
      (begin
        (place-on-ready-queue! (lambda () (apply-cont cont val)))
        (run-next-thread)
      )
      (begin
        (decrement-timer!)
        (if (null? cont)
          ; `cont` should be always non-empty
          (begin
            (eopl:printf "End of Computation.~%")
            val
          )
          (let ([saved-cont (cdr cont)])
            (cases frame (car cont)
              (zero1-frame ()
                (apply-cont saved-cont
                  (bool-val (zero? (expval->num val)))
                )
              )
              (let-exp-frame (vars exps rvars vals body saved-env)
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
                    (cons
                      (let-exp-frame (cdr vars) (cdr exps) (cons (car vars) rvars) (cons val vals) body saved-env)
                      saved-cont
                    )
                  )
                )
              )
              (if-test-frame (exp2 exp3 saved-env)
                (if (expval->bool val)
                  (value-of/k exp2 saved-env saved-cont)
                  (value-of/k exp3 saved-env saved-cont)
                )
              )
              (binary-op1-frame (op exp2 saved-env)
                (value-of/k exp2 saved-env
                  (cons (binary-op2-frame op val) saved-cont)
                )
              )
              (binary-op2-frame (op val1)
                (let ([num1 (expval->num val1)] [num2 (expval->num val)])
                  (apply-cont saved-cont
                    (num-val (op num1 num2))
                  )
                )
              )
              (rator-frame (rands saved-env)
                (let ([proc1 (expval->proc val)])
                  (if (null? rands)
                    (apply-procedure/k proc1 '() saved-cont)
                    (value-of/k (car rands) saved-env
                      (let ([rds (cdr rands)])
                        (if (null? rds)
                          (cons (rand2-frame proc1) saved-cont)
                          (cons (rand1-frame rds proc1 saved-env) saved-cont)
                        )
                      )
                    )
                  )
                )
              )
              (rand1-frame (rands proc1 saved-env)
                (value-of/k (car rands) saved-env
                  (cases proc proc1
                    (procedure (vars body p-saved-env)
                      (let (
                          [rds (cdr rands)]
                          [proc2 (procedure (cdr vars) body (extend-env (car vars) (newref val) p-saved-env))]
                        )
                        (if (null? rds)
                          (cons (rand2-frame proc2) saved-cont)
                          (cons (rand1-frame rds proc2 saved-env) saved-cont)
                        )
                      )
                    )
                  )
                )
              )
              (rand2-frame (proc1)
                (apply-procedure/k proc1 (list val) saved-cont)
              )
              (cons1-frame (exp2 saved-env)
                (value-of/k exp2 saved-env
                  (cons (cons2-frame val saved-env) saved-cont)
                )
              )
              (cons2-frame (val1 saved-env)
                (let ([any1 (expval->any val1)] [any2 (expval->any val)])
                  (apply-cont saved-cont
                    (list-val (cons any1 any2))
                  )
                )
              )
              (null?-frame (saved-env)
                (apply-cont saved-cont
                  (bool-val (null? (expval->list val)))
                )
              )
              (car-frame (saved-env)
                (let ([list1 (expval->list val)])
                  (apply-cont saved-cont
                    (any->expval (car list1))
                  )
                )
              )
              (cdr-frame (saved-env)
                (let ([list1 (expval->list val)])
                  (apply-cont saved-cont
                    (any->expval (cdr list1))
                  )
                )
              )
              (list1-frame (exps saved-env)
                (value-of/k (list-exp exps) saved-env
                  (cons (list2-frame val saved-env) saved-cont)
                )
              )
              (list2-frame (val1 saved-env)
                (apply-cont saved-cont (list-val (cons (expval->any val1) (expval->list val)))
                )
              )
              (set-rhs-frame (saved-env var)
                (begin
                  (setref! (apply-env saved-env var) val)
                  (apply-cont saved-cont (num-val 27))
                )
              )
              (begin-frame (exps saved-env)
                (if (null? exps)
                  (apply-cont saved-cont val)
                  (value-of/k (car exps) saved-env
                    (cons (begin-frame (cdr exps) saved-env) saved-cont)
                  )
                )
              )
              (print-frame ()
                (begin
                  (println val)
                  (apply-cont saved-cont (num-val 26))
                )
              )
              (spawn-frame ()
                (let ([proc1 (expval->proc val)])
                  (place-on-ready-queue!
                    (lambda ()
                      (apply-procedure/k proc1
                        '(num-val 28)
                        (list (end-subthread-frame))
                      )
                    )
                  )
                  (apply-cont saved-cont (num-val 73))
                )
              )
              (end-main-thread-frame ()
                (set-final-answer! val)
                (run-next-thread)
              )
              (end-subthread-frame ()
                (run-next-thread)
              )
              (wait-frame ()
                (wait-for-mutex
                  (expval->mutex val)
                  (lambda () (apply-cont saved-cont (num-val 52)))
                )
              )
              (signal-frame ()
                (signal-mutex
                  (expval->mutex val)
                  (lambda () (apply-cont saved-cont (num-val 53)))
                )
              )
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
        (value-of/k exp1 env (cons (zero1-frame) cont))
      )
      (let-exp (vars exps body)
        (if (null? vars)
          (value-of/k body env cont)
          (value-of/k (car exps) env
            (cons
              (let-exp-frame (cdr vars) (cdr exps) (list (car vars)) '() body env)
              cont
            )
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (cons (if-test-frame exp2 exp3 env) cont)
        )
      )
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (cons (binary-op1-frame - exp2 env) cont)
        )
      )
      (mul-exp (exp1 exp2)
        (value-of/k exp1 env
          (cons (binary-op1-frame * exp2 env) cont)
        )
      )
      (call-exp (rator rands)
        (value-of/k rator env
          (cons (rator-frame rands env) cont)
        )
      )
      (emptylist-exp () (apply-cont cont (list-val '())))
      (cons-exp (exp1 exp2)
        (value-of/k exp1 env
          (cons (cons1-frame exp2 env) cont)
        )
      )
      (null?-exp (exp1)
        (value-of/k exp1 env
          (cons (null?-frame env) cont)
        )
      )
      (car-exp (exp1)
        (value-of/k exp1 env
          (cons (car-frame env) cont)
        )
      )
      (cdr-exp (exp1)
        (value-of/k exp1 env
          (cons (cdr-frame env) cont)
        )
      )
      (list-exp (exps)
        (if (null? exps)
          (apply-cont cont (list-val '()))
          (value-of/k (car exps) env
            (cons (list1-frame (cdr exps) env) cont)
          )
        )
      )
      (assign-exp (var exp1)
        (value-of/k exp1 env
          (cons (set-rhs-frame env var) cont)
        )
      )
      (begin-exp (exp1 exps)
        (value-of/k exp1 env
          (cons (begin-frame exps env) cont)
        )
      )
      (print-exp (exp1)
        (value-of/k exp1 env (cons (print-frame) cont))
      )
      (spawn-exp (exp1)
        (value-of/k exp1 env (cons (spawn-frame) cont))
      )
      (mutex-exp ()
        (apply-cont cont (mutex-val (new-mutex)))
      )
      (wait-exp (exp1)
        (value-of/k exp1 env (cons (wait-frame) cont))
      )
      (signal-exp (exp1)
        (value-of/k exp1 env (cons (signal-frame) cont))
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
