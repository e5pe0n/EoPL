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
  var x, y; {
    x = 3;
    y = 4;
    print +(x, y)
  }
")
; #(struct:num-val 7)
; End of Computation.

(run "
  var x, y, z; {
    x = 3;
    y = 4;
    z = 0;
    while not (zero? (x)) {
      z = +(z, y);
      x = -(x, 1)
    };
    print z
  }
")
; #(struct:num-val 12)
; End of Computation.

(run "
  var x; {
    x = 3;
    print x;
    var x; {
      x = 4;
      print x
    };
    print x
  }
")
; #(struct:num-val 3)
; #(struct:num-val 4)
; #(struct:num-val 3)
; End of Computation.

(run "
  var f, x; {
    f = proc(x, y) *(x, y);
    x = 3;
    print (f 4 x)
  }
")
; #(struct:num-val 12)
; End of Computation.
