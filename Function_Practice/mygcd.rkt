#lang racket

(define mygcd
  (lambda (a b)
    (if (zero? b)
    a
    (mygcd b (modulo a b)))))