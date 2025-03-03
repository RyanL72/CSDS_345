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

(define maxnumber-cps
  (lambda (lis current-max return)
    (if (null? lis)
        (return current-max)
        (maxnumber-cps (cdr lis)
          (if (> (car lis) current-max) (car lis) current-max)
          return))))

(define maxnumber
  (lambda (lis)
    (if (null? lis)
        '()
        (maxnumber-cps lis (car lis) (lambda (v) v)))))

(define partialsums*-helper
  (lambda (lst return)
    (letrec ([loop
              (lambda (lst sum sublists return)
                (if (null? lst)
                    (return sum (reverse sublists))
                    (let ((elem (car lst)))
                      (cond
                        ((number? elem)
                         (loop (cdr lst) (+ sum elem) sublists return))
                        ((list? elem)
                         (partialsums*-helper elem
                           (lambda (sub-sum sub-processed)
                             (loop (cdr lst) (+ sum sub-sum)
                                   (cons sub-processed sublists)
                                   return))))
                        (else
                         (loop (cdr lst) sum sublists return))))))])
      (loop lst 0 '() return))))

(define partialsums*
  (lambda (lst)
    (partialsums*-helper lst
      (lambda (sum processed)
        (cons sum processed)))))

(define count-atoms
  (lambda (lst return)
    (if (null? lst)
        (return 0)
        (let ((elem (car lst)))
          (cond
            ((list? elem)
             (count-atoms elem
               (lambda (cnt1)
                 (count-atoms (cdr lst)
                   (lambda (cnt-rest)
                     (return (+ cnt1 cnt-rest)))))))
            ((not (list? elem))
             (count-atoms (cdr lst)
               (lambda (cnt-rest)
                 (return (+ 1 cnt-rest)))))
            (else
             (count-atoms (cdr lst) (lambda (cnt-rest) (return cnt-rest)))))))))

(define drop-atoms-cps
  (lambda (lst k return)
    (if (or (zero? k) (null? lst))
        (return lst)
        (drop-atoms-cps (cdr lst) (- k 1) return))))

(define trimatoms
  (lambda (lst1 lst2)
    (count-atoms lst1
      (lambda (k)
        (drop-atoms-cps lst2 k (lambda (res) res))))))

(define exchange-cps
  (lambda (lst replacements return)
    (if (null? lst)
        (return '() replacements)
        (let ((elem (car lst)))
          (if (list? elem)
              (exchange-cps elem replacements
                (lambda (new-sublist replacements-after)
                  (exchange-cps (cdr lst) replacements-after
                    (lambda (new-rest final-replacements)
                      (return (cons new-sublist new-rest) final-replacements)))))
              (exchange-cps (cdr lst) (cdr replacements)
                (lambda (new-rest remaining)
                  (return (cons (car replacements) new-rest) remaining))))))))

(define exchange
  (lambda (lst replacements)
    (exchange-cps lst replacements (lambda (new-list remaining) new-list))))

(define append-cps
  (lambda (lst1 lst2 return)
    (if (null? lst1)
        (return lst2)
        (append-cps (cdr lst1) lst2
          (lambda (rest)
            (return (cons (car lst1) rest)))))))

(define removeallatoms-cps
  (lambda (lst return)
    (letrec ([loop
              (lambda (lst structure atoms return)
                (if (null? lst)
                    (return (reverse structure) (reverse atoms))
                    (let ((elem (car lst)))
                      (cond
                        ((list? elem)
                         (removeallatoms-cps elem
                           (lambda (sub-structure sub-atoms)
                             (append-cps sub-atoms atoms
                               (lambda (combined-atoms)
                                 (loop (cdr lst)
                                       (cons sub-structure structure)
                                       combined-atoms
                                       return))))))
                        ((not (list? elem))
                         (loop (cdr lst) structure (cons elem atoms) return))
                        (else
                         (loop (cdr lst) (cons elem structure) atoms return))))))])
      (loop lst '() '() return))))

(define removeallatoms
  (lambda (lst)
    (removeallatoms-cps lst (lambda (structure atoms) (cons structure atoms)))))

(define removesubsequence*-helper
  (lambda (pattern lst return)
    (if (null? pattern)
        (return lst)
        (if (null? lst)
            (return '())
            (let ((elem (car lst)))
              (cond
                ((list? elem)
                 (removesubsequence*-helper pattern elem
                   (lambda (new-sublist)
                     (removesubsequence*-helper pattern (cdr lst)
                       (lambda (new-rest)
                         (return (cons new-sublist new-rest)))))))
                ((not (list? elem))
                 (if (equal? elem (car pattern))
                     (removesubsequence*-helper (cdr pattern) (cdr lst) return)
                     (removesubsequence*-helper pattern (cdr lst)
                       (lambda (new-rest)
                         (return (cons elem new-rest)))))))
                (else
                 (removesubsequence*-helper pattern (cdr lst) return)))))))

(define removesubsequence*
  (lambda (pattern lst)
    (removesubsequence*-helper pattern lst (lambda (res) res))))

(define (find-first-last x lst index first-occ last-occ)
  (if (null? lst)
      (values first-occ last-occ)
      (if (equal? (car lst) x)
          (if (not first-occ)
              (find-first-last x (cdr lst) (add1 index) index index)
              (find-first-last x (cdr lst) (add1 index) first-occ index))
          (find-first-last x (cdr lst) (add1 index) first-occ last-occ))))

(define collapse-x
  (lambda (x lst)
    (call/cc (lambda (exit)
      (let-values ([(first-occ last-occ) (find-first-last x lst 0 #f #f)])
        (if (or (not first-occ) (equal? first-occ last-occ))
            lst
            (append (take lst first-occ)
                    (list (list-ref lst first-occ))
                    (drop lst (add1 last-occ)))))))))

(define (take lst n)
  (if (or (zero? n) (null? lst))
      '()
      (cons (car lst) (take (cdr lst) (sub1 n)))))

(define (drop lst n)
  (if (or (zero? n) (null? lst))
      lst
      (drop (cdr lst) (sub1 n))))

(define (index-of x lst)
  (letrec ([loop (lambda (lst i)
                    (cond ((null? lst) 0)
                          ((equal? (car lst) x) i)
                          (else (loop (cdr lst) (+ i 1)))))])
    (loop lst 1)))

(define xindex
  (lambda (x lst)
    (call/cc (lambda (exit)
      (letrec ([xindex-helper
                (lambda (lst)
                  (if (list? lst)
                      (if (member x lst)
                          (list (index-of x lst))
                          (map (lambda (elem)
                                 (if (list? elem)
                                     (xindex-helper elem)
                                     elem))
                               lst))
                      lst))])
        (xindex-helper lst))))))
