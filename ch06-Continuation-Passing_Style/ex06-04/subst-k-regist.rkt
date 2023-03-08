#lang eopl

(require "../utils.rkt")

(define new 'uninitialized)
(define old 'uninitialized)
(define slist 'uninitialized)
(define sexp 'uninitialized)
(define val 'uninitialized)
(define cont 'uninitialized)

(define subst
  (lambda (new1 old1 slist1)
    (set! cont (end-cont))
    (set! new new1)
    (set! old old1)
    (set! slist slist1)
    (subst/k)
  )
)

(define subst/k
  (lambda ()
    (if (null? slist)
      (begin
        (set! val '())
        (apply-cont)
      )
      (begin
        (set! cont (subst1-cont new old slist cont))
        (set! slist (cdr slist))
        (subst/k)
      )
    )
  )
)

(define subst-in-s-exp/k
  (lambda ()
    (if (symbol? sexp)
      (if (eqv? sexp old)
        (begin
          (set! val new)
          (apply-cont)
        )
        (begin
          (set! val sexp)
          (apply-cont)
        )
      )
      (begin
        (set! slist sexp)
        (subst/k)
      )
    )
  )
)

(define-datatype continuation continuation?
  (end-cont)
  (subst1-cont
    (new symbol?)
    (old symbol?)
    (sexp (lambda (x) (or (list? x) (symbol? x))))
    (cont continuation?)
  )
  (subst2-cont
    (val? (lambda (x) (or (list? x) (symbol? x))))
    (cont continuation?)
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
      (subst1-cont (new old sexp1 saved-cont)
        (set! cont (subst2-cont val saved-cont))
        (set! sexp (car sexp1))
        (subst-in-s-exp/k)
      )
      (subst2-cont (val1 saved-cont)
        (set! cont saved-cont)
        (set! val (cons val val1))
        (apply-cont)
      )
    )
  )
)

(println (subst 'a 'b '(a b c b)))
; "End of Computation."
; "This sentense should appear only once."
; (a a c a)

(println (subst 'a 'b '((b c) (b () d))))
; "End of Computation."
; "This sentense should appear only once."
; ((a c) (a () d))


