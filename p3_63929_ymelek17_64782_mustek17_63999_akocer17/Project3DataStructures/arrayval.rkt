(module arrayval (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "store.scm")
  
  (provide (all-defined-out))
  
  
   (define array-val?
    (lambda (value)
      (if (list? value)
          (if (null? value) #t
              (reference? (car value)))
          #f)))
  ; Recursively add to the array if exist, else create one.
  (define newarray
    (lambda (length value)
      (if (= length 0)
          '()
          (cons (newref value) (newarray (- length 1) value)))))

  ; Find the index's location in the array and update the object in the
  ; reference with given value.
  (define update-array
    (lambda (array index value)
      (setref! (list-ref array index) value)))

  ; Returning the corresponding value from linkedlist array type.
  (define read-array
    (lambda (array index)
      (deref (list-ref array index))))
  
  
  
  )
