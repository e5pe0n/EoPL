#lang eopl

(require "../utils.rkt")

(define loi 'uninitialized)
(define val 'uninitialized)
(define cont 'uninitialized)

(define list-sum
  (lambda (loi1)
    (set! cont (end-cont))
    (set! loi loi1)
    (list-sum/k)
  )
)

(define list-sum/k
  (lambda ()
    (if (null? loi)
      (begin
        (set! val 0)
        (apply-cont)
      )
      (begin
        (set! cont (list-sum-cont (car loi) cont))
        (set! loi (cdr loi))
        (list-sum/k)
      )
    )
  )
)

(define-datatype continuation continuation?
  (end-cont)
  (list-sum-cont
    (n integer?)
    (saved-cont continuation?)
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
      (list-sum-cont (n saved-cont)
        (set! cont saved-cont)
        (set! val (+ n val))
        (apply-cont)
      )
    )
  )
)

(println (list-sum '(1 2 3)))
; "End of Computation."
; "This sentense should appear only once."
; 6

