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

(define list-sum-cont
  (lambda (n cont)
    (lambda (val)
      (cont (+ n val))
    )
  )
)

(define apply-cont
  (lambda (cont val)
    (cont val)
  )
)

(println (list-sum '(1 2 3)))
; "End of Computation."
; "This sentense should appear only once."
; 6
