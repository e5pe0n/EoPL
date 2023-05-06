#lang eopl

(provide (all-defined-out))

; Any -> Bool
(define s-val?
  (lambda (x)
    #t
  )
)

; SchemeVal -> Bool
(define identifier?
  (lambda (x)
    (symbol? x)
  )
)

; SchemeVal -> ()
(define println
  (lambda (x)
    (eopl:printf "~s~%" x)
  )
)

; Var -> ()
(define report-no-binding-found
  (lambda (err-name search-var)
    (eopl:error err-name ": No binding for ~s" search-var)
  )
)

; List * Sym -> List
(define join
  (lambda (xs sep)
    (if (null? xs)
      xs
      (let loop ([xs xs])
        (if (= (length xs) 1)
          xs
          (cons (car xs) (cons sep (loop (cdr xs))))
        )
      )
    )
  )
)

; ((T2 * T1) -> T2) * Listof(T1) * T2 -> T2
(define reduce
  (lambda (fn xs init-val)
    (if (null? xs)
      init-val
      (reduce fn (cdr xs) (fn init-val (car xs)))
    )
  )
)

; Listof(List) -> Listof(List)
(define zip
  (lambda (xss)
    (if (or (null? xss) (any null? xss))
      '()
      (cons (map car xss) (zip (map cdr xss)))
    )
  )
)

; (T1 -> Bool) * Listof(T1) -> Bool
(define all
  (lambda (pred xs)
    (if (null? xs)
      #t
      (let ([b (pred (car xs))])
        (if b (all pred (cdr xs)) #f)
      )
    )
  )
)

; (T1 -> Bool) * Listof(T1) -> Bool
(define any
  (lambda (pred xs)
    (if (null? xs)
      #f
      (let ([b (pred (car xs))])
        (if b #t (any pred (cdr xs)))
      )
    )
  )
)
