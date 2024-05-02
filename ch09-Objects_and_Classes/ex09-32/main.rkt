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

(println (run "
interface tree
  method int sum()
  method bool equal(t: tree)
  method bool equal-interior-node(t: interior-node)
  method bool equal-leaf-node(t: leaf-node)

class interior-node extends object implements tree
  field tree left
  field tree right
  method void initialize(l: tree, r: tree)
    begin
      set left = l; set right = r
    end
  method tree getleft() left
  method tree getright() right
  method int sum()
    +(send left sum(), send right sum())
  method bool equal(t: tree)
    send t equal-interior-node(self)
  method bool equal-interior-node(t: interior-node)
    if send left equal(send t getleft())
    then send right equal(send t getright())
    else zero?(1)
  method bool equal-leaf-node(t: leaf-node)
    zero?(1)

class leaf-node extends object implements tree
  field int value
  method void initialize(v: int)
    set value = v
  method int sum() value
  method int getvalue() value
  method bool equal(t: tree)
    send t equal-leaf-node(self)
  method bool equal-interior-node(t: interior-node)
    zero?(1)
  method bool equal-leaf-node(t: leaf-node)
    zero?(-(value, send t getvalue()))

let o1 = new interior-node(
          new interior-node(new leaf-node(3), new leaf-node(4)),
          new leaf-node(5)
        )
in list(
  send o1 sum(),
  if send o1 equal(o1)
  then 100
  else 200
)
"))
; #(struct:list-val (#(struct:num-val 12) #(struct:num-val 100)))
