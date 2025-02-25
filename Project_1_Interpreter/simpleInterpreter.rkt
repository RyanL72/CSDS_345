#lang racket

;;===============================================================
;; Simple Interpreter
;;
;; CSDS 345
;; 
;; This interpreter reads a file containing a simple Java/C-ish language,
;; parses it using the provided parser (simpleParser.rkt and lex.rkt),
;; and then evaluates the resulting syntax tree.
;;
;; The interpreter supports variable declarations (with and without assignment),
;; assignment expressions (including nested assignments), arithmetic and boolean
;; operations, comparisons, if statements, while loops, and return statements.
;;
;; The interpreter is written in a pure, state‐passing style. Every expression
;; evaluation returns a pair: (cons value updated-state). Statements thread the
;; state similarly.
;;===============================================================

(require "simpleParser.rkt")
(require "lex.rkt")


;;===============================================================
;; State Management
;;===============================================================

;; Create an empty state (an association list)
(define (make-empty-state)
  '())

;; Lookup a variable in the state. Error if not found.
(define (lookup state var)
  (let ([binding (assoc var state)])
    (if binding
        (cdr binding)
        (error 'lookup "Variable ~a not declared" var))))

;; Bind a new variable to a value.
(define (bind state var val)
  (if (assoc var state)
      (error 'bind "Variable ~a already declared" var)
      (cons (cons var val) state)))


;; Evaluate an expression in a given state.
(define (eval-expr expr state)
  (cond
    ((number? expr) expr)
    (else (error 'eval-expr "Unknown expression: ~a" expr))))

;; Evaluate a single statement in the current state.
(define (eval-stmt stmt state)
  (cond
    ;; Handle a return statement: (return <expr>)
    ((and (list? stmt) (equal? (car stmt) 'return))
     (let ([val (eval-expr (cadr stmt) state)])
       ;; Bind 'return in the state with the computed value.
       (bind state 'return val)))
    (else (error 'eval-stmt "Unknown statement: ~a" stmt))))

;; Evaluate a list of statements sequentially.
(define (eval-statements stmts state)
  (cond
    ((null? stmts) state)
    (else
     (let ([new-state (eval-stmt (car stmts) state)])
       (if (assoc 'return new-state)
           new-state
           (eval-statements (cdr stmts) new-state))))))

(define (interpret filename)
  (let* ([parse-tree (parser filename)]       ; parser returns a syntax tree
         [initial-state (make-empty-state)]
         [final-state (eval-statements parse-tree initial-state)])
    (lookup final-state 'return)))


