#lang eopl

(require "../utils.rkt")

(define list-sum
  (lambda (loi)
    (list-sum/k loi
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

(define list-sum/k
  (lambda (loi cont)
    (if (null? loi)
      (cont 0)
      (list-sum/k (cdr loi)
        (lambda (val) (cont (+ (car loi) val)))
      )
    )
  )
)

(println (list-sum '(1 2 3)))
; "End of Computation."
; "This sentense should appear only once."
; 6
