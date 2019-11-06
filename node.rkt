#lang racket

(provide (all-defined-out))

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

(define (new-node-operator op left right)
  (node-operator '() op left right))

(struct node-unary-operator node-expr (op unary)
  #:transparent)

(define (new-node-unary-operator op unary)
  (node-unary-operator '() op unary))

(define (node-sub left right)
  (new-node-operator "-" left right))

(define (node-eq left right)
  (new-node-operator "==" left right))

(define (node-neq left right)
  (new-node-operator "!=" left right))

(define (node-lt left right)
  (new-node-operator "<" left right))

(define (node-le left right)
  (new-node-operator "<=" left right))

(define (node-addr node)
  (new-node-unary-operator "&" node))

(define (node-deref node)
  (new-node-unary-operator "*" node))

(define (node-sizeof node)
  (new-node-unary-operator "sizeof" node))

(define (node-add? node)
  (and (node-operator? node) (equal? (node-operator-op node) "+")))

(define (node-sub? node)
  (and (node-operator? node) (equal? (node-operator-op node) "-")))

(define (node-mul? node)
  (and (node-operator? node) (equal? (node-operator-op node) "*")))

(define (node-div? node)
  (and (node-operator? node) (equal? (node-operator-op node) "/")))

(define (node-eq? node)
  (and (node-operator? node) (equal? (node-operator-op node) "==")))

(define (node-neq? node)
  (and (node-operator? node) (equal? (node-operator-op node) "!=")))

(define (node-lt? node)
  (and (node-operator? node) (equal? (node-operator-op node) "<")))

(define (node-le? node)
  (and (node-operator? node) (equal? (node-operator-op node) "<=")))

(define (node-addr? node)
  (and (node-unary-operator? node) (equal? (node-unary-operator-op node) "&")))

(define (node-deref? node)
  (and (node-unary-operator? node) (equal? (node-unary-operator-op node) "*")))

(define (node-sizeof? node)
  (and (node-unary-operator? node) (equal? (node-unary-operator-op node) "sizeof")))

(define (reverse-compare op)
  (cond [(equal? op ">") "<"]
        [(equal? op "<") ">"]
        [(equal? op "<=") ">="]
        [(equal? op ">=") "<="]))

(module+ test
  (require rackunit))
