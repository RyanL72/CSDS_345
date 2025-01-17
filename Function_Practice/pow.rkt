#lang racket

(define pow
  (lambda (b p)
    (if (zero? p)
    1
    (* b (pow b (- p 1)))))) 