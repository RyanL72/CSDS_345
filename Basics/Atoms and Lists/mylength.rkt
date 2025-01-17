#lang racket

;mylength: returns the length of a list (number of elements)
; (mylenth '()) -> 0
; (mylength '(a b c d) -> 4

(define mylength
  (lambda (list)
    (if (null? list)
        0
        (+ 1 (mylength (cdr list))))))