#lang racket

(require "node.rkt")

(provide
 int
 is-int?
 pointer-of
 array-of
 is-pointer?
 is-array?
 pointer-or-array?
 type-of
 base-type
 same-type?)

(struct type (type base)
  #:transparent)

(define int (type 'int '()))

(define (base-type node)
  (type-base (node-expr-type node)))

(define (array-of base)
  (type 'array base))

(define (pointer-of base)
  (type 'pointer base))

(define (type-of type-or-node)
  (if (type? type-or-node)
      (type-type type-or-node)
      (type-type (node-expr-type type-or-node))))

(define (is-int? type-or-node)
  (equal? (type-of type-or-node) 'int))

(define (is-pointer? type-or-node)
  (equal? (type-of type-or-node) 'pointer))

(define (is-array? type-or-node)
  (equal? (type-of type-or-node) 'array))

(define (pointer-or-array? type-or-node)
  (or (is-pointer? type-or-node) (is-array? type-or-node)))

(define (same-type? node-x node-y)
  (equal? (type-of node-x) (type-of node-y)))

(module+ test
  (require rackunit))
