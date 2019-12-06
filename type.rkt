#lang racket

(require "node.rkt")

(provide
 int
 int?
 pointer-of
 array-of
 pointer?
 array?
 pointer-or-array?
 type-of
 base-type
 same-type?
 type-conversion-array-to-pointer)

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

(define (int? type-or-node)
  (equal? (type-of type-or-node) 'int))

(define (pointer? type-or-node)
  (equal? (type-of type-or-node) 'pointer))

(define (array? type-or-node)
  (equal? (type-of type-or-node) 'array))

(define (pointer-or-array? type-or-node)
  (or (pointer? type-or-node) (array? type-or-node)))

(define (type-conversion-array-to-pointer type)
  (if (array? type)
      (pointer-of (type-base type))
      type))

(define (same-type? node-x node-y)
  (equal? (type-of node-x) (type-of node-y)))

(module+ test
  (require rackunit))
