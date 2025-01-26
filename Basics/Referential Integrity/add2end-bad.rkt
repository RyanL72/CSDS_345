; explicit add the element to the end of the list adding a new cons box to the list no in racket I run this with r5r5
(define add2end-bad
  (lambda (x lis)
    (if (null? lis)
        (cons x '()))
        (begin (add2end-bad-helper x lis) lis)))

; ! indicates a method that does not follow good practce. Modified the array instead of returnning a new one. Destroys data.
(define add2end-bad-helper
  (lambda (x lis)
    (if (null? (cdr lis))
    (set-cdr! lis (cons x '()))
    (add2end-bad-helper x (cdr lis)))))