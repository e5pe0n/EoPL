#lang eopl

(require "../utils.rkt")

(define var 'uninitialized)
(define exp 'uninitialized)
(define val 'uninitialized)
(define cont 'uninitialized)

(define occurs-free?
  (lambda (var1 exp1)
    (set! cont (end-cont))
    (set! var var1)
    (set! exp exp1)
    (occurs-free?/k)
  )
)

(define occurs-free?/k
  (lambda ()
    (cond
      ((symbol? exp)
        (begin
          (set! val (eqv? var exp))
          (apply-cont)
        )
      )
      ((eqv? (car exp) 'lambda)
        (begin
          (set! cont (occurs-free?1-cont var exp cont))
          (set! exp (caddr exp))
          (apply-cont)
        )
      )
      (else
        (begin
          (set! cont (occurs-free?2-cont var exp cont))
          (set! exp (car exp))
          (occurs-free?/k)
        )
      )
    )
  )
)

(define-datatype continuation continuation?
  (end-cont)
  (occurs-free?1-cont
    (var symbol?)
    (exp (lambda (x) (or (list? x) (identifier? x))))
    (cont continuation?)
  )
  (occurs-free?2-cont
    (var symbol?)
    (exp (lambda (x) (or (list? x) (identifier? x))))
    (cont continuation?)
  )
  (occurs-free?3-cont
    (val boolean?)
    (cont continuation?)
  )
)

(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont ()
        (begin
          (println "End of Computation.")
          (println "This sentense should appear only once.")
          val
        )
      )
      (occurs-free?1-cont (var exp saved-cont)
        (begin
          (set! cont saved-cont)
          (set! val
            (and
              (not (eqv? var (car (cadr exp))))
              val
            )
          )
          (apply-cont)
        )
      )
      (occurs-free?2-cont (var exp saved-cont)
        (begin
          (set! cont (occurs-free?3-cont val saved-cont))
          (set! exp (cadr exp))
          (occurs-free?/k)
        )
      )
      (occurs-free?3-cont (val1 saved-cont)
        (begin
          (set! cont saved-cont)
          (set! val (or val1 val))
          (apply-cont)
        )
      )
    )
  )
)

(println (occurs-free? 'x 'x))
; "End of Computation."
; "This sentense should appear only once."
; #t

(println (occurs-free? 'x 'y))
; "End of Computation."
; "This sentense should appear only once."
; #f

(println (occurs-free? 'x '(lambda (x) (x y))))
; "End of Computation."
; "This sentense should appear only once."
; #f

(println (occurs-free? 'x '(lambda (y) (x y))))
; "End of Computation."
; "This sentense should appear only once."
; #t

(println (occurs-free? 'x '((lambda (x) x) (x y))))
; "End of Computation."
; "This sentense should appear only once."
; #t

(println (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))))
; "End of Computation."
; "This sentense should appear only once."
; #t
