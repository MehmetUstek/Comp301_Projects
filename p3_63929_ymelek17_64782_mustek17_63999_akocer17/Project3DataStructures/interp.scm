(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require "arrayval.rkt")
  (require "queueval.rkt")
  (require "stackval.rkt")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        ;; PART A: ARRAYS
        (newarray-exp (exp1 exp2)
           (let ((length (expval->num (value-of exp1 env)))
                (value (value-of exp2 env)))
             (array-val (newarray length value))
             ))

        (update-array-exp (exp1 exp2 exp3)
           (let ((array (expval->array (value-of exp1 env)))
                 (index (expval->num (value-of exp2 env)))
                 (value (value-of exp3 env)))
             (update-array array index value)
           ))

        (read-array-exp (exp1 exp2)
           (let ((arr (expval->array (value-of exp1 env)))
                 (index (expval->num (value-of exp2 env))))
             (read-array arr index)))

        ;; PART B: STACKS
        (newstack-exp ()
           (let ((stack (newarray 1000 (bool-val #f))))
                (array-val stack)))

        (stack-push-exp (exp1 exp2)
           (let ((stack (expval->array (value-of exp1 env)))
                 (value (value-of exp2 env)))
                 (update-array stack (expval->num (stack-size stack)) value)))


        (stack-size-exp (exp1)
           (let ((stack (expval->array (value-of exp1 env))))
             (stack-size stack)))

        (empty-stack?-exp (exp1)
           (let ((stack (expval->array (value-of exp1 env))))
             (bool-val (empty-stack? stack))))            
                        
        (stack-pop-exp (exp1)
           (let ((stack (expval->array (value-of exp1 env))))
             (if (empty-stack? stack) (num-val -1)
                   (let ((pop-value (read-array stack (- (expval->num (stack-size stack)) 1))))
                      (update-array stack (- (expval->num (stack-size stack)) 1) (bool-val #f))
                       pop-value))))

        (stack-top-exp (exp1)
           (let ((stack (expval->array (value-of exp1 env))))
             (if (empty-stack? stack) (num-val -1)
                 (let ((top-value (read-array stack (- (expval->num (stack-size stack)) 1))))
                   top-value))))

        (print-stack-exp (exp1)
           (let ((stack (expval->array (value-of exp1 env))))
              (print-stack stack 0)))

        ;; PART C: QUEUES
        ;newqueue() returns an empty queue.
       (newqueue-exp ()
           (let ((queue-array (newarray 1000 (bool-val #f))))
               (array-val queue-array)))

        ;queue-push(queue, val) adds the element val to the stack queue
        (queue-push-exp (exp1 exp2)
           (let ((queue (expval->array (value-of exp1 env)))
                 (value (value-of exp2 env)))
                 (update-array queue (expval->num (queue-size queue)) value)
                 ))

        
        ;queue-pop(queue) removes the first element of the queue queue and returns its value.                
        (queue-pop-exp (exp1)
           (let ((queue (expval->array (value-of exp1 env))))
             (let ((popped (read-array queue 0)))
                 (begin
                   (shift_left queue 0)
                   (update-array queue (- (expval->num (queue-size queue)) 1) (bool-val #f)))
               popped)))
     

        ;queue-size(queue) returns the number of elements in the queue queue
        (queue-size-exp (exp1)
           (let ((queue (expval->array (value-of exp1 env))))
             (queue-size queue)))

        ;empty-queue?(queue) returns true if there is no element inside the queue queue and false otherwise.
        (empty-queue?-exp (exp1)
           (let ((queue (expval->array (value-of exp1 env))))
             (bool-val (empty-queue? queue))))            
       
        ;queue-top(queue) returns the value of the first element in the stack queue without removal
        (queue-top-exp (exp1)
           (let ((queue (expval->array (value-of exp1 env))))
             (read-array queue 0)
             ))
        
        ;print-queue(queue) prints the elements in the queue queue
        (print-queue-exp (exp1)
           (let ((queue (expval->array (value-of exp1 env))))
              (print-queue queue 0)))
        
        )))
  

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))

     
  )
  


  
