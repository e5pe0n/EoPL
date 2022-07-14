#lang eopl

(require "lang.rkt")
(require "interp.rkt")
(require "utils.rkt")

; String -> ExpVal
(define run
  (lambda (string)
    (result-of-program (scan&parse string))
  )
)

(run "
  var x, y = 1, 2; {
    print x;
    print y;
    var x, y = 100, 200; {
      print x;
      print y
    };
    print x;
    print y
  }
")
; #(struct:num-val 1)
; #(struct:num-val 2)
; #(struct:num-val 100)
; #(struct:num-val 200)
; #(struct:num-val 1)
; #(struct:num-val 2)

; the scope of a variable does NOT include initializer for variables
; declared lafter in the same block statement.
;   NG: var x, y = 1, x; { ... }
