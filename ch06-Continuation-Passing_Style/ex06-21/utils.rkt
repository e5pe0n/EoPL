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

; ((T) -> Bool) * Listof(T) -> Listof(T)
(define filter
  (lambda (pred xs)
    (let loop ([xs xs])
      (if (null? xs)
        xs
        (let ([head (car xs)] [tail (cdr xs)])
          (if (pred head)
            (cons head (loop tail))
            (loop tail)
          )
        )
      )
    )
  )
)

; List -> List
(define copy-list
  (lambda (xs)
    (let loop ([xs xs])
      (if (null? xs)
        xs
        (cons (car xs) (loop (cdr xs)))
      )
    )
  )
)

; ((T) -> Bool) * Listof(T) -> Bool
(define every?
  (lambda (pred xs)
    (let loop ([xs xs])
      (if (null? xs)
        #t
        (and (pred (car xs)) (loop (cdr xs)))
      )
    )
  )
)

; (T -> Bool) * List -> Int | Bool
(define list-index
  (lambda (pred xs)
    (let loop ([i 0] [xs xs])
      (if (null? xs)
        #f
        (if (pred (car xs))
          i
          (loop (+ i 1) (cdr xs))
        )
      )
    )
  )
)

; (T -> Bool) * List -> Int | Bool
(define list-last-index
  (lambda (pred xs)
    (let loop ([i 0] [xs xs] [last-index -1])
      (if (null? xs)
        (if (= last-index -1) #f last-index)
        (loop (+ i 1) (cdr xs) (if (pred (car xs)) i last-index))
      )
    )
  )
)

; List * Int * SchemeVal -> List
(define list-set
  (lambda (xs i x)
    (if (null? xs)
      '()
      (cons
        (if (= i 0)
          x
          (car xs)
        )
        (list-set (cdr xs) (- i 1) x)
      )
    )
  )
)

(define fresh-identifier
  (let ([sn 0])
    (lambda (var)
      (set! sn (+ sn 1))
      (string->symbol
        (string-append
          (symbol->string var)
          "%"
          (number->string sn)
        )
      )
    )
  )
)
