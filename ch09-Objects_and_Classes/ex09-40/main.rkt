#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "interp.rkt")
(require "checker.rkt")

(define run
  (lambda (string)
    (let ([pgm (scan&parse string)])
      (type-of-program pgm)
      (value-of-program pgm)
    )
  )
)

; (println (run "
; interface tree
;   method int sum()
;   method bool equal(t: tree)

; class interior-node extends object implements tree
;   field tree left
;   field tree right
;   method void initialize(l: tree, r: tree)
;     begin
;       set left = l; set right = r
;     end
;   method tree getleft() left
;   method tree getright() right
;   method int sum()
;     +(send left sum(), send right sum())
;   method bool equal(t: tree)
;     if instanceof t interior-node
;     then if send left equal(send cast t interior-node getleft())
;           then send right equal(send cast t interior-node getright())
;           else zero?(1)
;     else zero?(1)

; class leaf-node extends object implements tree
;   field int value
;   method void initialize(v: int)
;     set value = v
;   method int sum() value
;   method int getvalue() value
;   method bool equal(t: tree)
;     if instanceof t leaf-node
;     then zero?(-(value, send cast t leaf-node getvalue()))
;     else zero?(1)

; let o1 = new interior-node(
;           new interior-node(new leaf-node(3), new leaf-node(4)),
;           new leaf-node(5)
;         )
; in list(
;   send o1 sum(),
;   if send o1 equal(o1)
;   then 100
;   else 200
; )
; "))
; #(struct:list-val (#(struct:num-val 12) #(struct:num-val 100)))

(println (run "
  class c1 extends object
    field int x = 0
    field bool b = zero?(1)
    method int initialize() 1
    method int getx() x
    method bool getb() b

  let o1 = new c1()
  in begin
    print(send o1 getx());
    print(send o1 getb())
  end
"))
; #(struct:num-val 0)
; #(struct:bool-val #f)
; 29
