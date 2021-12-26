(import datatype)

(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack
    (val s-val?)
    (stk stack?)
  )
)
(define s-val?
  (lambda (v)
    #t
  )
)
(define empty-stack?
  (lambda (s)
    (cases stack s
      (empty-stack () #t)
      (non-empty-stack (val stk) #f)
    )
  )
)
(define push
  (lambda (v s)
    (non-empty-stack v s)
  )
)
(define pop
  (lambda (s)
    (cases stack s
      (empty-stack () (report-empty-stack 'pop))
      (non-empty-stack (val stk) stk)
    )
  )
)
(define top
  (lambda (s)
    (cases stack s
      (empty-stack () (report-empty-stack 'top))
      (non-empty-stack (val stk) val)
    )
  )
)
(define report-empty-stack
  (lambda (from)
    (error from "stack is empty.")
  )
)

(define s1
  (push 3 (push 2 (push 1 (empty-stack))))
)
(print (top s1))  ; 3
(print (top (pop (pop s1))))  ; 1