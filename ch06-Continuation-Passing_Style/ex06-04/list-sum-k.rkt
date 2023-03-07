#lang eopl

(require "../utils.rkt")

(define list-sum
  (lambda (loi)
    (list-sum/k loi (end-cont))
  )
)

(define list-sum/k
  (lambda (loi cont)
    (if (null? loi)
      (apply-cont cont 0)
      (list-sum/k (cdr loi) (list-sum-cont (car loi) cont))
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
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
        (begin
          (println "End of Computation.")
          (println "This sentense should appear only once.")
          val
        )
      )
      (list-sum-cont (n saved-cont)
        (apply-cont saved-cont (+ n val))
      )
    )
  )
)

(println (list-sum '(1 2 3)))
; "End of Computation."
; "This sentense should appear only once."
; 6
