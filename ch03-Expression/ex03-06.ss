(import datatype)

(define-datatype env env?
  (empty-env)
  (extend-env
    (saved-var symbol?)
    (saved-val s-val?)
    (saved-env env?)
  )
)
(define s-val?
  (lambda (x)
    #t
  )
)
(define apply-env
  (lambda (e search-var)
    (cases env e
      (empty-env () (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)
        )
      )
    )
  )
)
(define has-binding?
  (lambda (e search-var)
    (cases env e
      (empty-env () #f)
      (extend-env (saved-var saved-val saved-env)
        (or (eqv? search-var saved-var)
          (has-binding? saved-env search-var)
        )
      )
    )
  )
)
(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env ": No binding for -s" search-var)
  )
)
(define report-invalid-env
  (lambda (e)
    (error 'apply-env ": Bad environment=-s" e)
  )
)

(define-datatype program program?
  (a-program
    (exp1 expression?)
  )
)
(define-datatype expression expression?
  (const-exp
    (num number?)
  )
  (bool-exp
    (bool boolean?)
  )
  (vlist-exp
    (lst1 vlist?)
  )
  (emptyvlist-exp)
  (elist-exp
    (lst1 list?)
  )
  (null?-exp
    (exp1 expression?)
  )
  (diff-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (add-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (mul-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (quo-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (zero?-exp
    (exp1 expression?)
  )
  (if-exp
    (exp1 expression?)
    (exp2 expression?)
    (exp3 expression?)
  )
  (var-exp
    (var identifier?)
  )
  (let-exp
    (var identifier?)
    (exp1 expression?)
    (body expression?)
  )
  (minus-exp
    (exp1 expression?)
  )
  (not-exp
    (exp1 expression?)
  )
  (equal?-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (less?-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (greater?-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (cons-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (car-exp
    (exp1 expression?)
  )
  (cdr-exp
    (exp1 expression?)
  )
  (cond-exp
    (exp-pairs list?)
  )
)
(define identifier?
  (lambda (x)
    (symbol? x)
  )
)

(define-datatype vlist vlist?
  (emptyvlist)
  (non-empty-vlist
    (first expval?)
    (rest vlist?)
  )
)
(define report-empty-list-error
  (lambda (name)
    (error name "empty list")
  )
)


(define-datatype expval expval?
  (num-val
    (num number?)
  )
  (bool-val
    (bool boolean?)
  )
  (list-val
    (lst1 vlist?)
  )
)
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else report-expval-extractor-error 'num val)
    )
  )
)
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else report-expval-extractor-error 'bool val)
    )
  )
)
(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (lst1) lst1)
      (else report-expval-extractor-error 'list val)
    )
  )
)
(define report-expval-extractor-error
  (lambda (name val)
    (error name "invalid syntax: -s" val)
  )
)

(define init-env
  (lambda ()
    (extend-env 'i (num-val 1)
      (extend-env 'v (num-val 5)
        (extend-env 'x (num-val 10)
          (empty-env)
        )
      )
    )
  )
)

(define run
  (lambda (string)
    (value-of-program (span&parse string))
  )
)
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))
      )
    )
  )
)
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (bool-exp (bool) (bool-val bool))
      (emptyvlist-exp () (list-val (emptyvlist)))
      (vlist-exp (lst1) (list-val lst1))
      (elist-exp (lst1)
        (value-of
          (if (null? lst1)
            (emptyvlist-exp)
            (cons-exp (car lst1) (elist-exp (cdr lst1)))
          )
          env
        )
      )
      (null?-exp (exp1)
        (let ([val1 (value-of exp1 env)])
          (let ([_lst (expval->list val1)])
            (cases vlist _lst
              (emptylist () (bool-val #t))
              (non-empty-vlist (first rest) (bool-val #f))
            )
          )
        )
      )
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
        (let
          (
            [val1 (value-of exp1 env)]
            [val2 (value-of exp2 env)]
          )
          (let
            (
              [num1 (expval->num val1)]
              [num2 (expval->num val2)]
            )
            (num-val (- num1 num2))
          )
        )
      )
      (add-exp (exp1 exp2)
        (value-of (diff-exp exp1 (minus-exp exp2)) env)
      )
      (mul-exp (exp1 exp2)
        (value-of
          (let-exp 'expression1 (if-exp (less?-exp exp1 exp2) exp1 exp2)
            (let-exp 'expression2 (if-exp (less?-exp exp1 exp2) exp2 exp1)
              (if-exp (zero?-exp (var-exp 'expression1))
                (const-exp 0)
                (add-exp (var-exp 'expression2) (mul-exp (diff-exp (var-exp 'expression1) (const-exp 1)) (var-exp 'expression2)))
              )
            )
          )
          env
        )
      )
      (quo-exp (exp1 exp2)
        (value-of
          (if-exp (less?-exp exp1 exp2)
            (const-exp 0)
            (add-exp (const-exp 1) (quo-exp (diff-exp exp1 exp2) exp2))
          )
          env
        )
      )
      (zero?-exp (exp1)
        (let ([val1 (value-of exp1 env)])
          (let ([num1 (expval->num val1)])
            (if (zero? num1)
              (bool-val #t)
              (bool-val #f)
            )
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (let ([val1 (value-of exp1 env)])
          (if (expval->bool val1)
            (value-of exp2 env)
            (value-of exp3 env)
          )
        )
      )
      (let-exp (var exp1 body)
        (let ([val1 (value-of exp1 env)])
          (value-of body
            (extend-env var val1 env)
          )
        )
      )
      (minus-exp (exp1)
        (value-of (diff-exp (const-exp 0) exp1) env)
      )
      (not-exp (exp1)
        (bool-val (not (expval->bool (value-of exp1 env))))
      )
      (equal?-exp (exp1 exp2)
        (value-of (zero?-exp (diff-exp exp1 exp2)) env)
      )
      (less?-exp (exp1 exp2)
        (let ([val1 (value-of exp1 env)] [val2 (value-of exp2 env)])
          (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
            (bool-val (< num1 num2))
          )
        )
      )
      (greater?-exp (exp1 exp2)
        (value-of
          (if-exp (equal?-exp exp1 exp2)
            (bool-exp #f)
            (not-exp (less?-exp exp1 exp2))
          )
          env
        )
      )
      (cons-exp (exp1 exp2)
        (list-val (non-empty-vlist
          (value-of exp1 env)
          (let ([val1 (value-of exp2 env)])
            (expval->list val1)
          )
        ))
      )
      (car-exp (exp1)
        (let ([val1 (value-of exp1 env)])
          (let ([_lst (expval->list val1)])
            (cases vlist _lst
              (non-empty-vlist (first rest) first)
              (else (report-empty-list-error "value-of car-exp"))
            )
          )
        )
      )
      (cdr-exp (exp1)
        (let ([val1 (value-of exp1 env)])
          (let ([_lst (expval->list val1)])
            (cases vlist _lst
              (non-empty-vlist (first rest) (list-val rest))
              (else (report-empty-list-error "value-of cdr-exp"))
            )
          )
        )
      )
      (cond-exp (exp-pairs)
        (if (null? exp-pairs)
          (error "value-of cond-exp; no satisfied condition")
          (let ([fst (car exp-pairs)])
            (let ([left-val (value-of (car fst) env)])
              (value-of
                (if (expval->bool left-val)
                  (cadr fst)
                  (cond-exp (cdr exp-pairs))
                )
                env
              )
            )
          )
        )
      )
    )
  )
)

(define vlist->list
  (lambda (mylst)
    (cases vlist mylst
      (emptyvlist () '())
      (non-empty-vlist (first rest)
        (cons
          (cases expval first
            (num-val (num) num)
            (bool-val (bool) bool)
            (list-val (lst1) (vlist->list lst1))
          )
          (vlist->list rest)
        )
      )
    )
  )
)

(print
  (expval->num
    (value-of
      (let-exp 'x (const-exp 3)
        (cond-exp (list
          (list (equal?-exp (var-exp 'x) (const-exp 1)) (const-exp 10))
          (list (equal?-exp (var-exp 'x) (const-exp 2)) (const-exp 20))
          (list (equal?-exp (var-exp 'x) (const-exp 3)) (const-exp 30))
        ))
      )
      (empty-env)
    )
  )
) ; 30
; (print
;   (vlist->list
;     (expval->list
;       (value-of
;         (let-exp 'x (const-exp 4)
;           (elist-exp (list (var-exp 'x) (diff-exp (var-exp 'x) (const-exp 1)) (diff-exp (var-exp 'x) (const-exp 3))))
;         )
;         (empty-env)
;       )
;     )
;   )
; ) ; (4 3 1)
; (print
;   (vlist->list
;     (non-empty-vlist (num-val 1)
;       (non-empty-vlist (list-val
;         (non-empty-vlist (num-val 10)
;           (non-empty-vlist (bool-val #t) (emptylist))
;         ))
;         (non-empty-vlist (bool-val #f)
;           (emptylist)
;         )
;       )
;     )
;   )
; )
; (define e1
;   (cons-exp
;     (const-exp 1)
;     (cons-exp
;       (cons-exp
;         (const-exp 2)
;         (cons-exp
;           (const-exp 3)
;           (emptyvlist-exp)
;         )
;       )
;       (cons-exp
;         (const-exp 4)
;         (emptyvlist-exp)
;       )
;     )
;   )
; )
; (print
;   (vlist->list
;     (expval->list
;       (value-of
;         e1
;         (empty-env)
;       )
;     )
;   )
; ) ; (1 (2 3) 4)
; (print
;   (expval->num
;     (value-of
;       (car-exp e1)
;       (empty-env)
;     )
;   )
; ) ; 1
; (print
;   (vlist->list
;     (expval->list
;       (value-of
;         (car-exp (cdr-exp e1))
;         (empty-env)
;       )
;     )
;   )
; ) ; (2 3)
; (define e2
;   (let-exp 'x (const-exp 4)
;     (cons-exp
;       (var-exp 'x)
;       (cons-exp
;         (cons-exp
;           (diff-exp (var-exp 'x) (const-exp 1))
;           (emptyvlist-exp)
;         )
;         (emptyvlist-exp)
;       )
;     )
;   )
; )
; (print
;   (vlist->list
;     (expval->list
;       (value-of e2 (empty-env))
;     )
;   )
; ) ; (4 (3))
; (print
;   (expval->bool
;     (value-of (null?-exp e2) (empty-env))
;   )
; ) ; #f
; (print
;   (expval->bool
;     (value-of (null?-exp (emptyvlist-exp)) (empty-env))
;   )
; ) ; #t

; (define e1
;   (minus-exp
;     (diff-exp
;       (minus-exp (const-exp 5))
;       (const-exp 9)
;     )
;   )
; )
; (print (expval->num (value-of e1 (empty-env)))) ; 14
; (print (expval->bool
;   (value-of
;     (equal?-exp e1 (const-exp 14))
;     (empty-env)
;   )
; )) ; #t

; (newline)

; (print (expval->bool (value-of
;   (less?-exp e1 (const-exp 13))
;   (empty-env)
; ))) ; #f
; (print (expval->bool (value-of
;   (less?-exp e1 (const-exp 14))
;   (empty-env)
; ))) ; #f
; (print (expval->bool (value-of
;   (less?-exp e1 (const-exp 15))
;   (empty-env)
; ))) ; #t

; (newline)

; (print (expval->bool (value-of
;   (greater?-exp e1 (const-exp 13))
;   (empty-env)
; ))) ; #t
; (print (expval->bool (value-of
;   (greater?-exp e1 (const-exp 14))
;   (empty-env)
; ))) ; #f
; (print (expval->bool (value-of
;   (greater?-exp e1 (const-exp 15))
;   (empty-env)
; ))) ; #f

; (newline)

; (print (expval->num (value-of
;   (add-exp e1 e1)
;   (empty-env)
; ))) ; 28

; (newline)

; (print (expval->num (value-of
;   (mul-exp e1 (const-exp 3))
;   (empty-env)
; ))) ; 42

; (newline)

; (print (expval->num (value-of
;   (quo-exp e1 (const-exp 6))
;   (empty-env)
; ))) ; 2
; (print (expval->num (value-of
;   (quo-exp e1 (const-exp 7))
;   (empty-env)
; ))) ; 2
; (print (expval->num (value-of
;   (quo-exp e1 (const-exp 8))
;   (empty-env)
; ))) ; 1