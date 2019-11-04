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

(define (base-type type)
  (type-base type))

(define (pointer-of base)
  (type 'pointer base))

(define (type-of node)
  (type-type (node-expr-type node)))

(define (is-int? node)
  (equal? (type-of node) 'int))

(define (is-pointer? node)
  (equal? (type-of node) 'pointer))

(define (same-type? node-x node-y)
  (equal? (type-of node-x) (type-of node-y)))

(module+ test
  (require rackunit))
