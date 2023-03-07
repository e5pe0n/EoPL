#lang eopl

(require "utils.rkt")

; Sym * Listof(Sym) -> Listof(Sym)
(define remove-first
  (lambda (s los)
    (if (null? los)
      los
      (if (eqv? (car los) s)
        (cdr los)
        (cons (car los) (remove-first s (cdr los)))
      )
    )
  )
)

(println (remove-first 'a '(a b c)))  ; (b c)
(println (remove-first 'b '(e f g)))  ; (e f g)
(println (remove-first 'a4 '(c1 a4 c1 a4))) ; (c1 c1 a4)
(println (remove-first 'x '())) ; ()
