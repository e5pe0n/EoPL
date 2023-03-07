#lang eopl

(require "../utils.rkt")

(define remove-first
  (lambda (s los)
    (remove-first/k s los (end-cont))
  )
)

(define remove-first/k
  (lambda (s los cont)
    (if (null? los)
      (apply-cont cont los)
      (if (eqv? (car los) s)
        (apply-cont cont (cdr los))
        (remove-first/k s (cdr los) (remove-first-cont (car los) cont))
      )
    )
  )
)

(define-datatype continuation continuation?
  (end-cont)
  (remove-first-cont
    (s symbol?)
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
      (remove-first-cont (s saved-cont)
        (apply-cont saved-cont (cons s val))
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
