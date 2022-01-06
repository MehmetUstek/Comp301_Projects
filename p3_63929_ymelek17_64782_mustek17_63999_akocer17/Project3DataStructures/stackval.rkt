(module arrayval (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "store.scm")
  (require "arrayval.rkt")
  (require "data-structures.scm")
  
  (provide (all-defined-out))


  (define-datatype stack stack?
    (stack-type (stack array-val?)))

  (define stack->array
    (lambda (stk)
      (cases stack stk  
        (stack-type (stack) stack) 
        (else (expval-extractor-error 'stack stk)))))

  (define stack-size
    (lambda (array)
      (num-val (end-of-stack array 0))))

  (define end-of-stack
    (lambda (array index)
      (if (equal? (deref (list-ref array index)) (bool-val #f))
          index
          (end-of-stack array (+ index 1)))))

  (define empty-stack?
    (lambda (array)
      (eq? (expval->num (stack-size array)) 0)))

  (define print-stack
    (lambda (array n)
      (begin
        (display (read-array array n))) 
        (print-stack array (+ n 1))))
  )
