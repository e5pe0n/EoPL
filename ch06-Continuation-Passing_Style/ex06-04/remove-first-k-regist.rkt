#lang eopl

(require "../utils.rkt")

(define val 'uninitialized)
(define s 'uninitialized)
(define los 'uninitialized)
(define cont 'uninitialized)

(define remove-first
  (lambda (s1 los1)
    (begin
      (set! s s1)
      (set! los los1)
      (set! cont (end-cont))
      (remove-first/k)
    )
  )
)

(define remove-first/k
  (lambda ()
    (if (null? los)
      (begin
        (set! val los)
        (apply-cont)
      )
      (begin
        (if (eqv? (car los) s)
          (begin
            (set! val (cdr los))
            (apply-cont)
          )
          (begin
            (set! cont (remove-first-cont (car los) cont))
            (set! los (cdr los))
            (remove-first/k)
          )
        )
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
  (lambda ()
    (cases continuation cont
      (end-cont ()
        (begin
          (println "End of Computation.")
          (println "This sentense should appear only once.")
          val
        )
      )
      (remove-first-cont (s saved-cont)
        (begin
          (set! cont saved-cont)
          (set! val (cons s val))
          (apply-cont)
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


