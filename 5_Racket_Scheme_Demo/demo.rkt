; Ryan Lin

#lang racket

;multiplyby takes a number and a list of numbrs and returns a list that is the input list with each element multiplied by the input number.

(define multiplyby
  (lambda (x lis)
    (if (null? lis) '()
      (cons (* x (car lis)) (multiplyby x (cdr lis))))))

; maxnnumber takes a list of numbers that contains at least one number and returns the largest number in the list
(define maxnumber
  (lambda (lis)
    (cond
      ((null? lis) 0)
      ((> (car lis) (maxnumber (cdr lis))) (car lis))
      (else (maxnumber (cdr lis))))))

;removelast takes a list of atoms containing tat least one atom and returnns the same list minus the last atom of the list.
(define removelast
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((null? (cdr lis)) '())
      (else (cons (car lis) (removelast (cdr lis)))))))      

; crossmultiply takes two lists of numbers, each list represents a vector. Returns the outer product of the two vectors.
;               The outer product is a matrix ( a list of lists) and each list is the result of multiplying the second list by the corresponding value of the first list.

(define crossmultiply
  (lambda (x y)
    (cond
      ((null? x) '())
      ((null? y) '())
      ((list? x) (cons (crossmultiply (car x) y) (crossmultiply (cdr x) y)))
      ((list? y) (cons (crossmultiply x (car y)) (crossmultiply x (cdr y))))
      (else ( * x y)))))

;interleave3 takes three lists of atoms and returns a list where the elements are interleaved in order in the pattern: (list1, list2, list3, list1, list2, list3, ...).
;The lists do not have to be the same length. You can write this function without any helper functions.

(define interleave3
  (lambda (lis1 lis2 lis3)
    (cond
      ((and (null? lis1) (null? lis2) (null? lis3)) '())
      ((null? lis1) (interleave3 lis2 lis3 lis1))
      (else (cons (car lis1) (interleave3 lis2 lis3 (cdr lis1)))))))


;reverse* takes a list that may contain sublists and reverses the contents of the list.
;         The contents of each sublist is also reversed.

(define reverse*
  (lambda (lis)
    (cond
      ((null? lis) '()) 
      ((list? (car lis)) 
       (append (reverse* (cdr lis)) (list (reverse* (car lis)))))
      (else
       (append (reverse* (cdr lis)) (list (car lis))))))) 
       
;reverselists takes a list, possibly containing sublists. The outerlist is not reversed, but each sublist is reversed. 
;              Any sublists inside those lists should stay in the original order, but sublists of the sublists should be reversed. 
;              This pattern should repeat for as many layers of sublists are in the list.
(define (reverselists lst)
  (cond
    ((null? lst) '()) 
    ((list? (car lst))
     (cons (map reverselists (reverse (car lst))) (reverselists (cdr lst))))
    (else (cons (car lst) (reverselists (cdr lst))))))

;trimatoms takes a list, possibly containing sublists, and a list of atoms. 
;          It returns the list of atoms with the first k atoms of the list removed where k is the number of non-null atoms in the first list. 
;          You can write this function without any helper functions.
(define (trimatoms lst atoms)
  (let ((count (length (filter symbol? (flatten lst)))))
    (drop atoms count))) 

;partialsums* takes a list that may contain sublists. The output should have the same list structure as the input, 
;             but the car of each list (and each sublist) should be the sum of all numbers in that list. 
;             There should be no other values in the list. You can write this function without any helper functions.
(define (partialsums* lst)
  (cond
    ((null? lst) '()) 
    ((list? lst) 
     (let ((sum (apply + (filter number? (flatten lst))))) 
       (cons sum (map partialsums* lst))))
    (else '()))) 

;exchange takes a list (possibly containing sublists) and a list of atoms. 
;         You may assume that both lists contain the same number of non-null atoms. 
;         The output should be identical to the first list except that the atoms of the list should be the same as the atoms of the second list, in order.
(define (exchange lst atoms)
  (let exchange-helper ((lst lst) (atoms atoms))
    (cond
      ((null? lst) '()) 
      ((list? (car lst))
       (cons (exchange-helper (car lst) atoms) (exchange-helper (cdr lst) atoms)))
      ((symbol? (car lst))
       (cons (car atoms) (exchange-helper (cdr lst) (cdr atoms))))
      (else (cons (car lst) (exchange-helper (cdr lst) atoms))))))