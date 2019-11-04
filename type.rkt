#lang racket

(require "node.rkt")

(provide
 int
 is-int?
 pointer-of
 is-pointer?
 type-of
 base-type
 same-type?)

(struct type (type base)
  #:transparent)

(define int (type 'int '()))

(define (base-type node)
  (type-base (node-expr-type node)))

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

(define (same-type? node-x node-y)
  (equal? (type-of node-x) (type-of node-y)))

(module+ test
  (require rackunit))
