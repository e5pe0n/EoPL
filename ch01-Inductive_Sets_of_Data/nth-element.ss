(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define nth-element
  (lambda (lst n)
    (if (null? lst)
      (report-list-too-short n)
      (if (= n 0)
        (car lst)
        (nth-element (cdr lst) (- n 1))
      )
    )
  )
)

(define report-list-too-short
  (lambda (n)
    (error
      'nth-element
      (string-append
        "List too short by "
        (number->string (+ n 1))
        " elements."
      )
    )
  )
)

(print
  (nth-element '(a b c d e) 3)
)
(print
  (nth-element '(a b c d e) 5)
)