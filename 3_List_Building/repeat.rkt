#lang racket

;repeat : takes an atom and a non-negative integer and returns a list containing that number of copies of the atom.
(define repeat
  (lambda (x num)
  (if (<= num 0) '()
  (cons x (repeat x (- num 1))))))


