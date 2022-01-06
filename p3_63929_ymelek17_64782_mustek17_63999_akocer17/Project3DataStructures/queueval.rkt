(module arrayval (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "store.scm")
  (require "arrayval.rkt")
  (require "data-structures.scm")
  
  (provide (all-defined-out))
  
  (define-datatype queue queue?
    (queue-type (queue array-val?)))

  (define queue->array
    (lambda (q)
      (cases queue q  
        (queue-type (queue) queue) 
        (else (expval-extractor-error 'queue q)))))
  
  (define queue-size
    (lambda (arr)
      (num-val (end-of-queue arr 0))))
  
  (define end-of-queue
    (lambda (arr index)
      (if (equal? (deref (list-ref arr index)) (bool-val #f))
          index
          (end-of-queue arr (+ index 1)))))
  
(define empty-queue?
    (lambda (arr)
      (= (expval->num (queue-size arr)) 0)))
  

(define print-queue
    (lambda (array n)
      (begin
        (display (read-array array n))) 
        (print-queue array (+ n 1))))


  (define shift_left
    (lambda (array index )
      (if (equal? index (- (expval->num (queue-size array)) 1))
          (display "")
          (begin
            (update-array array index (read-array array (+ index 1)))
            (shift_left array (+ index 1))))))
  )
