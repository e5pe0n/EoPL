#lang eopl

(require "utils.rkt")
(require "data-structures.rkt")
(require "lang.rkt")
(require "errors.rkt")

(provide (all-defined-out))

; FinalAnswer = ExpVal
(define run-thread
  (lambda (th)
    (cases thread th
      (a-thread (proc1 th-id parent-th-id)
        (proc1)
      )
    )
  )
)

(define the-ready-queue 'uninitialized)
(define the-final-answer 'uninitialized)
(define the-max-time-slice 'uninitialized)
(define the-time-remaining 'uninitialized)

(define the-next-th-id 'uninitialized)
(define the-msg-queues '())


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

; Listof(ExpVal) * ExpVal -> Listof(ExpVal)
(define enqueue-msg
  (lambda (q msg) (append q (list msg)))
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


; () -> ()
(define initialize-next-th-id!
  (lambda (init-th-id)
    (set! the-next-th-id init-th-id)
  )
)

; () -> Int
(define get-next-th-id
  (lambda ()
    (let ([next-th-id the-next-th-id])
      (set! the-next-th-id (+ the-next-th-id 1))
      next-th-id
    )
  )
)

; Thread * Int -> Bool
(define match-th
  (lambda (th th-id)
    (cases thread th
      (a-thread (proc1 th-id1 parent-th-id1)
        (= th-id th-id1)
      )
    )
  )
)

; Int -> Bool
(define kill-th!
  (lambda (th-id)
    (let ([org-len (length the-ready-queue)])
      (set! the-ready-queue
        (filter the-ready-queue (lambda (th) (match-th th th-id)))
      )
      (not (= org-len (length the-ready-queue)))
    )
  )
)

; () -> ()
(define new-msg-queue
  (lambda ()
    (set! the-msg-queues (append the-msg-queues (list (empty-queue))))
  )
)

; Int -> Listof(ExpVal)
(define get-msg-queue
  (lambda (th-id)
    (list-ref the-msg-queues th-id)
  )
)

; Int * ExpVal -> Unspecified
(define send-msg
  (lambda (th-id msg)
    (set! the-msg-queues
      (let f ([i th-id] [qs the-msg-queues])
        (if (zero? i)
          (cons (enqueue-msg (car qs) msg) (cdr qs))
          (cons (car qs) (f (- i 1) (cdr qs)))
        )
      )
    )
  )
)

; () -> ()
(define initialize-the-msg-queues!
  (lambda ()
    (new-msg-queue)
  )
)

; () -> ()
(define initialize-the-stores!
  (lambda ()
    (new-store)
  )
)

; Int * Program -> FinalAnswer
(define value-of-program
  (lambda (timeslice pgm)
    (initialize-scheduler! timeslice)
    (initialize-next-th-id! 0)
    (initialize-the-msg-queues!)
    (initialize-the-stores!)
    (cases program pgm
      (a-program (exp1)
        (let ([th-id (get-next-th-id)])
          (value-of/k exp1 (init-env) (end-main-thread-cont) th-id th-id)
        )
      )
    )
  )
)

; Cont * ExpVal * Int * Int -> FinalAnswer
(define apply-cont
  (lambda (cont val th-id parent-th-id)
    (if (time-expired?)
      (begin
        (place-on-ready-queue!
          (a-thread
            (lambda () (apply-cont cont val th-id parent-th-id))
            th-id parent-th-id
          )
        )
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
              th-id
              parent-th-id
            )
          )
          (let-exp-cont (vars exps rvars vals body saved-env saved-cont)
            (if (null? exps)
              (value-of/k body
                (let f ([vrs rvars] [vls (cons val vals)])
                  (if (null? vrs)
                    saved-env
                    (extend-env (car vrs) (newref (car vls) th-id) (f (cdr vrs) (cdr vls)))
                  )
                )
                saved-cont
                th-id
                parent-th-id
              )
              (value-of/k (car exps)
                saved-env
                (let-exp-cont (cdr vars) (cdr exps) (cons (car vars) rvars) (cons val vals) body saved-env saved-cont)
                th-id
                parent-th-id
              )
            )
          )
          (if-test-cont (exp2 exp3 saved-env saved-cont)
            (if (expval->bool val)
              (value-of/k exp2 saved-env saved-cont th-id parent-th-id)
              (value-of/k exp3 saved-env saved-cont th-id parent-th-id)
            )
          )
          (binary-op1-cont (op exp2 saved-env saved-cont)
            (value-of/k exp2 saved-env
              (binary-op2-cont op val saved-cont)
              th-id
              parent-th-id
            )
          )
          (binary-op2-cont (op val1 saved-cont)
            (let ([num1 (expval->num val1)] [num2 (expval->num val)])
              (apply-cont saved-cont
                (num-val (op num1 num2))
                th-id
                parent-th-id
              )
            )
          )
          (rator-cont (rands saved-env saved-cont)
            (let ([proc1 (expval->proc val)])
              (if (null? rands)
                (apply-procedure/k proc1 '() saved-cont th-id parent-th-id)
                (value-of/k (car rands) saved-env
                  (let ([rds (cdr rands)])
                    (if (null? rds)
                      (rand2-cont proc1 saved-cont)
                      (rand1-cont rds proc1 saved-env saved-cont)
                    )
                  )
                  th-id
                  parent-th-id
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
                      [proc2 (procedure (cdr vars) body (extend-env (car vars) (newref val th-id) p-saved-env))]
                    )
                    (if (null? rds)
                      (rand2-cont proc2 saved-cont)
                      (rand1-cont rds proc2 saved-env saved-cont)
                    )
                  )
                )
              )
              th-id
              parent-th-id
            )
          )
          (rand2-cont (proc1 saved-cont)
            (apply-procedure/k proc1 (list val) saved-cont th-id parent-th-id)
          )
          (cons1-cont (exp2 saved-env saved-cont)
            (value-of/k exp2 saved-env
              (cons2-cont val saved-env saved-cont)
              th-id
              parent-th-id
            )
          )
          (cons2-cont (val1 saved-env saved-cont)
            (let ([any1 (expval->any val1)] [any2 (expval->any val)])
              (apply-cont saved-cont
                (list-val (cons any1 any2))
                th-id
                parent-th-id
              )
            )
          )
          (null?-cont (saved-env saved-cont)
            (apply-cont saved-cont
              (bool-val (null? (expval->list val)))
              th-id
              parent-th-id
            )
          )
          (car-cont (saved-env saved-cont)
            (let ([list1 (expval->list val)])
              (apply-cont saved-cont
                (any->expval (car list1))
                th-id
                parent-th-id
              )
            )
          )
          (cdr-cont (saved-env saved-cont)
            (let ([list1 (expval->list val)])
              (apply-cont saved-cont
                (any->expval (cdr list1))
                th-id
                parent-th-id
              )
            )
          )
          (list1-cont (exps saved-env saved-cont)
            (value-of/k (list-exp exps) saved-env
              (list2-cont val saved-env saved-cont)
              th-id
              parent-th-id
            )
          )
          (list2-cont (val1 saved-env saved-cont)
            (apply-cont saved-cont
              (list-val (cons (expval->any val1) (expval->list val)))
              th-id
              parent-th-id
            )
          )
          (set-rhs-cont (saved-env var saved-cont)
            (begin
              (setref! (apply-env saved-env var) val th-id)
              (apply-cont saved-cont (num-val 27) th-id parent-th-id)
            )
          )
          (begin-cont (exps saved-env saved-cont)
            (if (null? exps)
              (apply-cont saved-cont val th-id parent-th-id)
              (value-of/k (car exps) saved-env
                (begin-cont (cdr exps) saved-env saved-cont)
                th-id
                parent-th-id
              )
            )
          )
          (print-cont (saved-cont)
            (begin
              (println val)
              (apply-cont saved-cont (num-val 26) th-id parent-th-id)
            )
          )
          (spawn-cont (saved-cont)
            (let
              (
                [proc1
                  (cases proc (expval->proc val)
                    (procedure (vars body saved-env)
                      (procedure vars body (extend-env 'PARENT_TH_ID (newref (num-val th-id) th-id) saved-env)
                      )
                    )
                  )
                ]
                [child-th-id (get-next-th-id)]
              )
              (new-msg-queue)
              (new-store)
              (set-store! (copy-list (get-store th-id)) child-th-id)
              (place-on-ready-queue!
                (a-thread
                  (lambda ()
                    (apply-procedure/k proc1
                      (list (num-val child-th-id))
                      (end-subthread-cont)
                      child-th-id
                      th-id
                    )
                  )
                  child-th-id th-id
                )
              )
              (apply-cont saved-cont child-th-id th-id parent-th-id)
            )
          )
          (end-main-thread-cont ()
            (set-final-answer! val)
            (run-next-thread)
          )
          (end-subthread-cont ()
            (run-next-thread)
          )
          (kill-cont (saved-cont)
            (apply-cont saved-cont
              (bool-val (kill-th! (expval->num val)))
              th-id
              parent-th-id
            )
          )
          (send1-cont (exp2 saved-env saved-cont)
            (value-of/k exp2 saved-env
              (send2-cont val saved-cont)
              th-id
              parent-th-id
            )
          )
          (send2-cont (val1 saved-cont)
            (begin
              (send-msg (expval->num val1) val)
              (apply-cont saved-cont
                (num-val 7)
                th-id
                parent-th-id
              )
            )
          )
        )
      )
    )

  )
)

; Exp x Env * Cont * Int * Int -> FinalAnswer
(define value-of/k
  (lambda (exp env cont th-id parent-th-id)
    (cases expression exp
      (const-exp (num)
        (apply-cont cont (num-val num) th-id parent-th-id)
      )
      (var-exp (var)
        (apply-cont cont (deref (apply-env env var) th-id) th-id parent-th-id)
      )
      (proc-exp (vars body)
        (apply-cont cont
          (proc-val (procedure vars body env))
          th-id
          parent-th-id
        )
      )
      (letrec-exp (p-name b-vars p-body letrec-body)
        (value-of/k
          letrec-body
          (extend-env-rec p-name b-vars p-body env th-id)
          cont
          th-id
          parent-th-id
        )
      )
      (zero?-exp (exp1)
        (value-of/k exp1 env (zero1-cont cont) th-id parent-th-id)
      )
      (let-exp (vars exps body)
        (if (null? vars)
          (value-of/k body env cont th-id parent-th-id)
          (value-of/k (car exps) env
            (let-exp-cont (cdr vars) (cdr exps) (list (car vars)) '() body env cont)
            th-id
            parent-th-id
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (if-test-cont exp2 exp3 env cont)
          th-id
          parent-th-id
        )
      )
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (binary-op1-cont - exp2 env cont)
          th-id
          parent-th-id
        )
      )
      (mul-exp (exp1 exp2)
        (value-of/k exp1 env
          (binary-op1-cont * exp2 env cont)
          th-id
          parent-th-id
        )
      )
      (call-exp (rator rands)
        (value-of/k rator env
          (rator-cont rands env cont)
          th-id
          parent-th-id
        )
      )
      (emptylist-exp ()
        (apply-cont cont (list-val '()) th-id parent-th-id)
      )
      (cons-exp (exp1 exp2)
        (value-of/k exp1 env
          (cons1-cont exp2 env cont)
          th-id
          parent-th-id
        )
      )
      (null?-exp (exp1)
        (value-of/k exp1 env
          (null?-cont env cont)
          th-id
          parent-th-id
        )
      )
      (car-exp (exp1)
        (value-of/k exp1 env
          (car-cont env cont)
          th-id
          parent-th-id
        )
      )
      (cdr-exp (exp1)
        (value-of/k exp1 env
          (cdr-cont env cont)
          th-id
          parent-th-id
        )
      )
      (list-exp (exps)
        (if (null? exps)
          (apply-cont cont (list-val '()) th-id parent-th-id)
          (value-of/k (car exps) env
            (list1-cont (cdr exps) env cont)
            th-id
            parent-th-id
          )
        )
      )
      (assign-exp (var exp1)
        (value-of/k exp1 env
          (set-rhs-cont env var cont)
          th-id
          parent-th-id
        )
      )
      (begin-exp (exp1 exps)
        (value-of/k exp1 env
          (begin-cont exps env cont)
          th-id
          parent-th-id
        )
      )
      (print-exp (exp1)
        (value-of/k exp1 env (print-cont cont) th-id parent-th-id)
      )
      (spawn-exp (exp1)
        (value-of/k exp1 env (spawn-cont cont) th-id parent-th-id)
      )
      (yield-exp ()
        (begin
          (place-on-ready-queue!
            (a-thread
              (lambda ()
                (apply-cont cont (num-val 99) th-id parent-th-id))
              th-id parent-th-id
            )
          )
          (run-next-thread)
        )
      )
      (kill-exp (exp1)
        (value-of/k exp1 env (kill-cont cont) th-id parent-th-id)
      )
      (send-exp (exp1 exp2)
        (value-of/k exp1 env (send1-cont exp2 env cont) th-id parent-th-id)
      )
      (recv-exp ()
        (let ([msg-queue (get-msg-queue th-id)])
          (if (null? msg-queue)
            (begin
              (place-on-ready-queue!
                (a-thread
                  (lambda () (value-of/k exp env cont th-id parent-th-id))
                  th-id parent-th-id
                )
              )
              (run-next-thread)
            )
            (apply-cont cont (car msg-queue) th-id parent-th-id)
          )
        )
      )
    )
  )
)


; Proc * Listof(ExpVal) * Cont * Int * Int -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 vals cont th-id parent-th-id)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of/k body
          (if (null? vars)
            saved-env
            (extend-env (car vars) (newref (car vals) th-id) saved-env)
          )
          cont
          th-id
          parent-th-id
        )
      )
    )
  )
)
