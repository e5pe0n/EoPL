#lang eopl

(require "../utils.rkt")

(define remove-first
  (lambda (s los)
    (remove-first/k s los
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

(define remove-first/k
  (lambda (s los cont)
    (if (null? los)
      (cont los)
      (if (eqv? (car los) s)
        (cont (cdr los))
        (remove-first/k s (cdr los)
          (lambda (val) (cont (cons (car los) val)))
        )
      )
    )
  )
)

(println (remove-first 'a '(a b c)))
; "End of Computation."
; "This sentense should appear only once."
; (b c)

(println (remove-first 'b '(e f g)))  ; (e f g)
; "End of Computation."
; "This sentense should appear only once."
; (e f g)

(println (remove-first 'a4 '(c1 a4 c1 a4))) ; (c1 c1 a4)
; "End of Computation."
; "This sentense should appear only once."
; (c1 c1 a4)

(println (remove-first 'x '())) ; ()
; "End of Computation."
; "This sentense should appear only once."
; ()

