#lang racket

(define multiplyby
  (lambda (val lis)
    (multiplyby-cps val lis (lambda (v) v))))

(define multiplyby-cps
  (lambda (val lis return)
    (if (null? lis)
        (return '())  
        (multiplyby-cps val (cdr lis)
          (lambda (rest) (return (cons (* (car lis) val) rest)))))))

(define crossmultiply
  (lambda (list1 list2)
    (crossmultiply-cps list1 list2 (lambda (v) v))))

(define crossmultiply-cps
  (lambda (list1 list2 return)
    (if (null? list1)
        (return '())  
        (multiplyby-cps (car list1) list2
          (lambda (row) 
            (crossmultiply-cps (cdr list1) list2
              (lambda (rest) (return (cons row rest)))))))))

(define maxnumber
  (lambda (lst)
    (maxnumber-cps lst (lambda (v) v))))

(define maxnumber-cps
  (lambda (lst return)
    (if (null? (cdr lst)) 
        (return (car lst))  
        (maxnumber-cps (cdr lst) 
          (lambda (rest-max) 
            (return (if (> (car lst) rest-max) (car lst) rest-max)))))))


