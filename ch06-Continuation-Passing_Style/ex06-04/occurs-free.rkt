#lang eopl

(require "../utils.rkt")

; Sym * Listof()
(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp)
        (eqv? var exp)
      )
      ((eqv? (car exp) 'lambda)
        (and
          (not (eqv? var (car (cadr exp))))
          (occurs-free? var (caddr exp))
        )
      )
      (else
        (or
          (occurs-free? var (car exp))
          (occurs-free? var (cadr exp))
        )
      )
    )
  )
)

(println (occurs-free? 'x 'x))  ; #t
(println (occurs-free? 'x 'y))  ; #f
(println (occurs-free? 'x '(lambda (x) (x y)))) ; #f
(println (occurs-free? 'x '(lambda (y) (x y)))) ; #t
(println (occurs-free? 'x '((lambda (x) x) (x y)))) ; #t
(println (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))))  ; #t
