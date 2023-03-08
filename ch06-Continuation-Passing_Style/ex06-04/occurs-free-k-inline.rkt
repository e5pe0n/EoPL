#lang eopl

(require "../utils.rkt")

(define occurs-free?
  (lambda (var exp)
    (occurs-free?/k var exp
      (lambda (val)
        (begin
          (println "End of Computation.")
          (println "This sentense should appear only once.")
          val
        )
      )
    )
  )
)

(define occurs-free?/k
  (lambda (var exp cont)
    (cond
      ((symbol? exp)
        (cont (eqv? var exp))
      )
      ((eqv? (car exp) 'lambda)
        (occurs-free?/k var (caddr exp)
          (lambda (val)
            (cont
              (and
                (not (eqv? var (car (cadr exp))))
                val
              )
            )
          )
        )
      )
      (else
        (occurs-free?/k var (car exp)
          (lambda (val1)
            (occurs-free?/k var (cadr exp)
              (lambda (val2)
                (cont (or val1 val2))
              )
            )
          )
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


