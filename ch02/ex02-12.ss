(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define empty-stack
  (lambda ()
    (lambda ()
      (error "empty stack")
    )
  )
)


(define push
  (lambda (x stack)
    (lambda (k)
      (cond
        ((eqv? k 'top) x)
        ((eqv? k 'pop) stack)
        (else (error "invalid argument"))
      )
    )
  )
)

(define pop
  (lambda (stack)
    (stack 'pop)
  )
)

(define top
  (lambda (stack)
    (stack 'top)
  )
)


(define s
  (push 'a
    (push 'b
      (push 'c
        (empty-stack)
      )
    )
  )
)
(print (top s)) ; a
(print (top (pop s))) ; b
(print (top (pop (pop s)))) ; c