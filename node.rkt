#lang racket

(provide (all-defined-out))

(require (for-syntax racket/syntax))

(struct node ()
  #:transparent)

(struct node-expr node (type)
  #:transparent)

(struct node-number node-expr (val)
  #:transparent)

(define (new-node-number val)
  (node-number '() val))

(struct node-local-variable node-expr (name)
  #:transparent)

(define (new-node-local-variable name)
  (node-local-variable '() name))

(struct node-assign node-expr (left right)
  #:transparent)

(define (new-node-assign left right)
  (node-assign '() left right))

(struct node-return node (expr)
  #:transparent)

(struct node-if node (conditional true-clause false-clause)
  #:transparent)

(struct node-while node (conditional body)
  #:transparent)

(struct node-for node (init conditional next body)
  #:transparent)

(struct node-block node (bodys)
  #:transparent)

(struct node-func-call node-expr (func args)
  #:transparent)

(define (new-node-func-call func args)
  (node-func-call '() func args))

(struct node-func-declaration node (name args body variables)
  #:transparent)

(struct node-variable-declaration node (var)
  #:transparent)

(struct node-operator node-expr (op left right)
  #:transparent)

(define-syntax (define-operator-node stx)
  (syntax-case stx ()
    [(_ name op)
     (with-syntax ([constructor (format-id #'name "node-~a" #'name)]
                   [operator-node? (format-id #'name "node-~a?" #'name)])
       #'(begin
           (define (constructor left right)
             (node-operator '() op left right))
           (define (operator-node? node)
             (and (node-operator? node) (equal? (node-operator-op node) op)))))]))

(define-operator-node add "+")
(define-operator-node sub "-")
(define-operator-node mul "*")
(define-operator-node div "/")
(define-operator-node eq "==")
(define-operator-node neq "!=")
(define-operator-node lt "<")
(define-operator-node le "<=")

(struct node-unary-operator node-expr (op node)
  #:transparent)

(define-syntax (define-unary-operator-node stx)
  (syntax-case stx ()
    [(_ name op)
     (with-syntax ([constructor (format-id #'name "node-~a" #'name)]
                   [operator-node? (format-id #'name "node-~a?" #'name)])
       #'(begin
           (define (constructor node)
             (node-unary-operator '() op node))
           (define (operator-node? node)
             (and (node-unary-operator? node) (equal? (node-unary-operator-op node) op)))))]))

(define-unary-operator-node addr "&")
(define-unary-operator-node deref "*")
(define-unary-operator-node sizeof "sizeof")

(define (reverse-compare op)
  (cond [(equal? op ">") "<"]
        [(equal? op "<") ">"]
        [(equal? op "<=") ">="]
        [(equal? op ">=") "<="]))

(module+ test
  (require rackunit))
