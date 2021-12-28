(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define empty-env
  (lambda ()
    '()
  )
)
(define empty-env?
  (lambda (env)
    (null? env)
  )
)
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)
  )
)
(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
      env
      (extend-env (car vars) (car vals) (extend-env* (cdr vars) (cdr vals) env))
    )
  )
)
(define apply-env
  (lambda (env search-var)
    (cond
      ((empty-env? env) (report-no-bouning-found search-var))
      ((eqv? (caar env) search-var) (cdar env))
      (else (apply-env (cdr env) search-var))
    )
  )
)
(define has-binding?
  (lambda (env var)
    (if (empty-env? env)
      #f
      (or (eqv? (caar env) var) (has-binding? (cdr env) var))
    )
  )
)
(define report-no-binding-found
  (lambda (search-var)
    (error (list 'apply-env ": No binding for" search-var))
  )
)
(define report-invalid-env
  (lambda (env)
    (error (list 'apply-env ": Bad environment=" env))
  )
)

; (print
;   (extend-env 'd 6
;     (extend-env 'y 8
;       (extend-env 'x 7
;         (extend-env 'y 14 (empty-env))
;       )
;     )
;   )
; ) ; ((d . 6) (y . 8) (x . 7) (y . 14))
(define env1 '((d . 6) (y . 8) (x . 7) (y . 14)))
; (print
;   (apply-env env1 'd)
; ) ; 6
; (print
;   (apply-env env1 'y)
; ) ; 8

; (print
;   (has-binding? env1 'y)
; ) ; #t
; (print
;   (has-binding? env1 'x)
; ) ; #t
; (print
;   (has-binding? env1 'z)
; ) ; #f

(print
  (extend-env* '(a b c) '(1 2 3) env1)
) ; ((a . 1) (b . 2) (c . 3) (d . 6) (y . 8) (x . 7) (y . 14))