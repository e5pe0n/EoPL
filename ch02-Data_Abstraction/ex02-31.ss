(import datatype)

(define-datatype prefix-exp prefix-exp?
  (const-exp
    (num integer?)
  )
  (diff-exp
    (operand1 prefix-exp?)
    (operand2 prefix-exp?)
  )
)

(define parse
  (lambda (lst)
    (if (null? lst)
      '()
      (cond
        ((integer? (car lst))
          (cons (const-exp (car lst)) (cdr lst))
        )
        ((eqv? (car lst) -)
          (let* ([res1 (parse (cdr lst))] [res2 (parse (cdr res1))])
            (cons
              (diff-exp
                (car res1)
                (car res2)
              )
              (cdr res2)
            )
          )
        )
      )
    )
  )
)
(define parse-exp
  (lambda (lst)
    (car (parse lst))
  )
)
(define prefix-exp-to-list
  (lambda (exp)
    (cases prefix-exp exp
      (const-exp (num) (list 'const-exp num))
      (diff-exp (operand1 operand2)
        (list 'diff-exp (prefix-exp-to-list operand1) (prefix-exp-to-list operand2))
      )
    )
  )
)
(print
  (prefix-exp-to-list
    (parse-exp (list - - 3 2 - 4 - 12 7))
  )
) ; (diff-exp (diff-exp (const-exp 3) (const-exp 2)) (diff-exp (const-exp 4) (diff-exp (const-exp 12) (const-exp 7))))