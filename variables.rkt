#lang racket

(provide
 (struct-out variable)
 make-variables
 assign-variable
 find-variable)

(define (variables-error msg)
  (raise-user-error
   'variables-error msg))

(struct variable (name type offset)
  #:transparent)

(define offset 0)

(define (new-offset)
  (set! offset (+ offset 8))
  offset)

(define (make-variables)
  (set! offset 0)
  (make-hash))

(define (find-variable variables name)
  (hash-ref variables name
            (lambda () (variables-error (format "variable: ~a is not found in ~a" name variables)))))

(define (assign-variable variables name type)
  (when (hash-has-key? variables name)
    (variables-error (format "variable: ~a is found in ~a" name variables)))
  (hash-set! variables name (variable name type (new-offset))))

(define (has-variable? variables name)
  (hash-has-key? variables name))

(module+ test
  (require rackunit))
