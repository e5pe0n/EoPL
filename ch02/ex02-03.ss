(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

; Diff-tree ::= (one) | (diff Diff-tree Diff-tree)

(define zero '(diff (one) (one)))
(define is-zero?
  (lambda (dt)
    (if (eqv? (car dt) 'one)
      #f
      (and (eqv? (caadr dt) 'one) (eqv? (caaddr dt) 'one))
    )
  )
)

(define one
  (lambda ()
    1
  )
)
(define diff
  (lambda (dt1 dt2)
    (- dt1 dt2)
  )
)

; (print
;   (diff (diff (one) (one)) (one))
; )
;  (print
;    (eval '(diff (diff (one) (one)) (one)))
;  )

; (print
;   (is-zero? '(one))
; )
; (print
;   (is-zero? '(diff (one) (one)))
; )
; (print
;   (is-zero? '(diff (diff (one) (one)) one))
; )

(define predecessor
  (lambda (dt)
    `(diff ,dt (one))
  )
)
(define successor
  (lambda (dt)
    `(diff ,dt ,(predecessor zero))
  )
)

; (print
;   (eval
;     (predecessor zero)
;   )
; )
; (print
;   (eval
;     (predecessor (predecessor (predecessor zero)))
;   )
; )
; (print
;   (eval
;     (successor (successor (successor zero)))
;   )
; )
; (print
;   (eval
;     (successor (predecessor (successor zero)))
;   )
; )
; (print
;   (eval
;     (predecessor (successor (predecessor zero)))
;   )
; )

(define diff-tree-plus
  (lambda (dt1 dt2)
    (let (
        [s (cadr dt2)] [t (caddr dt2)]
      )
      `(diff ,dt1 (diff ,t ,s))
    )
  )
)

(define repr
  (lambda (n)
    (let ([f (if (< n 0) predecessor successor)])
      (let g ([m (abs n)])
        (if (= m 0)
          zero
          (f (g (- m 1)))
        )
      )
    )
  )
)

; (print (repr 0))
; (print
;   (eval
;     (repr 1)
;   )
; )
; (print
;   (eval
;     (repr 2)
;   )
; )
; (print
;   (eval
;     (repr 100)
;   )
; )
; (print
;   (eval
;     (repr (- 0 1))
;   )
; )
; (print
;   (eval
;     (repr (- 0 2))
;   )
; )
; (print
;   (eval
;     (repr (- 0 100))
;   )
; )

(print
  (eval
    (diff-tree-plus (repr 0) (repr 0))
  )
)
(print
  (eval
    (diff-tree-plus (repr 0) (repr 1))
  )
)
(print
  (eval
    (diff-tree-plus (repr 1) (repr 0))
  )
)
(print
  (eval
    (diff-tree-plus (repr 1) (repr 1))
  )
)
(print
  (eval
    (diff-tree-plus (repr 1) (repr 2))
  )
)
(print
  (eval
    (diff-tree-plus (repr 2) (repr 1))
  )
)
(print
  (eval
    (diff-tree-plus (repr 2) (repr 2))
  )
)
(print
  (eval
    (diff-tree-plus (repr 0) (repr (- 0 1)))
  )
)
(print
  (eval
    (diff-tree-plus (repr (- 0 1)) (repr 0))
  )
)
(print
  (eval
    (diff-tree-plus (repr (- 0 1)) (repr (- 0 1)))
  )
)
(print
  (eval
    (diff-tree-plus (repr (- 0 1)) (repr (- 0 2)))
  )
)
(print
  (eval
    (diff-tree-plus (repr (- 0 2)) (repr (- 0 1)))
  )
)
(print
  (eval
    (diff-tree-plus (repr (- 0 2)) (repr (- 0 2)))
  )
)
(print
  (eval
    (diff-tree-plus (repr 386) (repr 975))
  )
)
(print
  (eval
    (diff-tree-plus (repr 362) (repr (- 0 975)))
  )
)