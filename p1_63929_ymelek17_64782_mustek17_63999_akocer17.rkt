#lang eopl

;;---------------------------------
;;Part A:
; First representation:
;Bool-list := ({Bool-exp}*)
;Bool-exp := Bool | Bool-list
;create-a: Int -> Bool-list
;is-zero-a?: Bool-list -> Bool
;successor-a: Bool-list-> Bool-list

; Second Representation
;create-b: Int -> String
;is-zero-b?: Int -> Bool
;successor-b: Int -> String
;;---------------------------------
;;Part B:

;for create-a:
(define create-a
    (lambda (x) 
      (cond
        ((eqv? x 0) '())
        (else
         (cons
          (list-helper (remainder x 10) (if (odd? (remainder x 10)) #f #t))
          (create-a (quotient x 10)))))))

 (define list-helper 
    (lambda (x bool) 
      (cond
        ((eqv? x 0) '())
        (else
         (cons bool (list-helper (- x 1) bool))))))

  (define is-zero-a?
    (lambda (x) (null? x)))

  (define successor-a
    (lambda (x)
      (cond
        ((< (length (car x)) 9) (cons (list-helper (+ 1 (length (car x))) (if (eq? (caar x) #f) #t #f)) (cdr x)))
        (else
         (cons '() (successor-a (cdr x)))))))


;for create-b:
(define lst
  (list
 'null
'Adana
'Adıyaman
'Afyon
'Ağrı
'Amasya
' Ankara
' Antalya
' Artvin
' Aydın
' Balıkesir
' Bilecik
' Bingöl
' Bitlis
' Bolu
' Burdur
' Bursa
' Çanakkale
' Çankırı
' Çorum
' Denizli
' Diyarbakır
' Edirne
' Elazığ
' Erzincan
' Erzurum
' Eskişehir
' Gaziantep
' Giresun
' Gümüşhane
' Hakkari
' Hatay
' Isparta
' Mersin
' İstanbul
' İzmir
' Kars
' Kastamonu
' Kayseri
' Kırklareli
' Kırşehir
' Kocaeli
' Konya
' Kütahya
' Malatya
' Manisa
' Kahramanmaraş
' Mardin
' Muğla
' Muş
' Nevşehir
' Niğde
' Ordu
' Rize
' Sakarya
' Samsun
' Siirt
' Sinop
' Sivas
' Tekirdağ
' Tokat
' Trabzon
' Tunceli
' Şanlıurfa
' Uşak
' Van
' Yozgat
' Zonguldak
' Aksaray
' Bayburt
' Karaman
' Kırıkkale
' Batman
' Şırnak
' Bartın
' Ardahan
' Iğdır
' Yalova
' Karabük
' Kilis
' Osmaniye
' Düzce))

(define null-item 'null)

(define create-b
    (lambda (x)
      (retrieve x lst)
      ))
(define retrieve
  (lambda (x lst)
   (list-ref lst x)))

  (define is-zero-b?
    (lambda (x) 
    (eqv? x 'null)
     ))

  (define successor-b
    (lambda (x)
      (create-b (- x 1))
      ))

;;---------------------------------
;;Part C:

;for create-a:

;create: constructor-->it creates a new representation using #t and #f values with respect to given numeric value. 
  
;is-zero?: observer (type: predicate) --> it extract information from values of the data type, 
;but it does not return any argument form the representation given- except true & false.
;if value of input equals to 0, then it returns #t, ow. returns false.
  
;successor():constructor--> creates a new representation with respect to given input value. 



;for create-b:

;create: constructor-->it creates a new representation using a list of strings which includes the names of cities in Turkey with respect to a given number between 1 and 81. 
  
;is-zero?: observer (type: predicate) --> it extract information from values of the data type, 
;but it does not return any argument form the representation given- except true & false.
;if value of input is in the range [1 ,81], then it returns #t, ow. returns false.
  
;successor():constructor--> creates a new representation with respect to given input value.


;;---------------------------------
  ;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Don't worry about the below function, we included it to test your implemented functions and display the result in the console
  ;; As you implement your functions you can Run (the button on the top right corner) to test your implementations
(define-syntax equal??
    (syntax-rules ()
      ((_ test-exp correct-ans)
       (let ((observed-ans test-exp))
         (if (not (equal? observed-ans correct-ans))
           (eopl:printf "Oops! ~s returned ~s, should have returned ~s~%" 'test-exp observed-ans correct-ans)
           (eopl:printf "Correct! ~s => ~s~%" 'test-exp correct-ans))))))


(display "First Representation Tests\n")
(equal?? (create-a 129) '((#f #f #f #f #f #f #f #f #f) (#t #t) (#f))); should return 
(equal?? (is-zero-a? (create-a 32)) #f) ; should return #f
(equal?? (is-zero-a? (create-a 0)) #t) ; should return #t
(equal?? (successor-a (create-a 129)) '(() (#f #f #f) (#f))) ; should return ?
(newline)

  
(display "Second Representation Tests\n")
(equal?? (create-b 6 ) 'Ankara)
(equal?? (is-zero-b? null-item) #t) ; should return #t
(equal?? (is-zero-b? (create-b 34)) #f) ; should return #f
(equal?? (successor-b 2) 'Adana) ; should return ?
(newline)

