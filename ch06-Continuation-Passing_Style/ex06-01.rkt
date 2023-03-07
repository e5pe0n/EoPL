#lang eopl

(require "utils.rkt")

(define n 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define pc 'uninitialized)

(define fact
  (lambda (arg-n)
    (set! cont (end-cont))
    (set! n arg-n)
    (set! pc fact/k)
    (trampoline!)
  )
)

(define trampoline!
  (lambda ()
    (if pc
      (begin
        (pc)
        (trampoline!)
      )
      val
    )
  )
)

(define fact/k
  (lambda ()
    (if (zero? n)
      (begin
        (set! val 1)
        (set! pc apply-cont)
      )
      (begin
        (set! cont (fact1-cont n cont))
        (set! n (- n 1))
        ; (set! pc fact/k)
      )
    )
  )
)

(define-datatype continuation continuation?
  (end-cont)
  (fact1-cont
    (n integer?)
    (cont continuation?)
  )
)

(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont ()
        (set! pc #f)
      )
      (fact1-cont (saved-n saved-cont)
        (set! cont saved-cont)
        (set! val (* val saved-n))
        ; (set! pc apply-cont)
        (apply-cont)
      )
    )
  )
)

(println (fact 5))  ; 120
