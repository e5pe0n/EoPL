#lang eopl

(require "../utils.rkt")

(define subst
  (lambda (new old slist)
    (subst/k new old slist (end-cont))
  )
)

(define subst/k
  (lambda (new old slist cont)
    (if (null? slist)
      (apply-cont cont '())
      (subst/k new old (cdr slist) (subst1-cont new old slist cont))
    )
  )
)

(define subst-in-s-exp/k
  (lambda (new old sexp cont)
    (if (symbol? sexp)
      (if (eqv? sexp old)
        (apply-cont cont new)
        (apply-cont cont sexp)
      )
      (subst/k new old sexp cont)
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

(define subst1-cont
  (lambda (new old sexp saved-cont)
    (lambda (val)
      (subst-in-s-exp/k new old (car sexp) (subst2-cont val saved-cont))
    )
  )
)

(define subst2-cont
  (lambda (val1 saved-cont)
    (lambda (val)
      (apply-cont saved-cont (cons val val1))
    )
  )
)

(define apply-cont
  (lambda (cont val)
    (cont val)
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


