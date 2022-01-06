#lang eopl

;; grammar for the LET language  

(provide (all-defined-out))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ;;(1) Add str-exp to the language
    (string ("'" letter (arbno letter) "'") string)
    ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)

    ;;(1) Add str-exp to the language
    (expression (string) str-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression (identifier) var-exp)

    ;;D(3)Add if-exp to the language.
    ;; we added arbno here, which stands for ’arbitrary number of something’
    (expression
     ("if" expression "then" expression
           (arbno "elif" expression "then" expression)
           "else" expression)
     if-exp)

    ;;;;
    (expression
     ("op" "("expression"," expression"," number")")
     op-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("exponential" "("expression"," expression ")")
     custom-exp)
    ))


;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
