#lang eopl

(require "../utils.rkt")

(define subst
  (lambda (new old slist)
    (subst/k new old slist
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

(define subst/k
  (lambda (new old slist cont)
    (if (null? slist)
      (cont '())
      (subst/k new old (cdr slist)
        (lambda (val1)
          (subst-in-s-exp/k new old (car slist)
            (lambda (val2)
              (cont (cons val2 val1))
            )
          )
        )
      )
    )
  )
)

(define subst-in-s-exp/k
  (lambda (new old sexp cont)
    (if (symbol? sexp)
      (if (eqv? sexp old)
        (cont new)
        (cont sexp)
      )
      (subst/k new old sexp cont)
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



