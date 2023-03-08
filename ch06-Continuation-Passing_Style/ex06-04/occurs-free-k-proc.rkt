#lang eopl

(require "../utils.rkt")

(define occurs-free?
  (lambda (var exp)
    (occurs-free?/k var exp (end-cont))
  )
)

(define occurs-free?/k
  (lambda (var exp cont)
    (cond
      ((symbol? exp)
        (apply-cont cont (eqv? var exp))
      )
      ((eqv? (car exp) 'lambda)
        (occurs-free?/k var (caddr exp) (occurs-free?1-cont var exp cont))
      )
      (else
        (occurs-free?/k var (car exp) (occurs-free?2-cont var exp cont))
      )
    )
  )
)

(define end-cont
  (lambda ()
    (lambda (val)
      (begin
        (println "End of Computation.")
        (println "This sentense should appear only once.")
        val
      )
    )
  )
)

(define occurs-free?1-cont
  (lambda (var exp saved-cont)
    (lambda (val)
      (apply-cont saved-cont
        (and
          (not (eqv? var (car (cadr exp))))
          val
        )
      )
    )
  )
)

(define occurs-free?2-cont
  (lambda (var exp saved-cont)
    (lambda (val)
      (occurs-free?/k var (cadr exp) (occurs-free?3-cont val saved-cont))
    )
  )
)

(define occurs-free?3-cont
  (lambda (val1 saved-cont)
    (lambda (val)
      (apply-cont saved-cont (or val1 val))
    )
  )
)


(define apply-cont
  (lambda (cont val)
    (cont val)
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

