#lang racket

(provide
 (struct-out variable)
 make-variables
 assign-variable
 variable-amount
 find-variable)

(define (variables-error msg)
  (raise-user-error
   'variables-error msg))

(struct variables (alist offset)
  #:transparent)

(struct variable (name type offset size)
  #:transparent)

(define (make-variables)
  (variables '() 0))

(define (assign-variable vars name type [size 0])
  (define offset (+ 8 (variables-offset vars)))
  (define alist-variable (cons name (variable name type offset size)))
  (when (has-variable? vars name)
    (variables-error (format "variable: ~a is already assigend in ~a" name (variables-alist vars))))
  (variables (cons alist-variable (variables-alist vars)) offset))

(define (has-variable? vars name)
  (assoc name (variables-alist vars)))

(define (find-variable vars name)
  (define as (has-variable? vars name))
  (unless as
    (variables-error
     (format "variable: ~a is found in ~a" name (variables-alist vars))))
  (cdr as))

(define (variable-amount vars)
  (for/fold
      ([sum 0])
      ([as (variables-alist vars)])
    (define size (variable-size (cdr as)))
    (+ sum (* 8 (if (= size 0) 1 size)))))

(module+ test
  (require rackunit)
  (define x (variable "x" 'int 8 0))
  (define vars (assign-variable (make-variables) "x" 'int))

  (test-case "make-variables"
    (test-equal? "create variables"
                 (make-variables)
                 (variables '() 0)))

  (test-case "assing-variable"
    (test-equal? "assign variable"
                 vars
                 (variables `(("x" . ,x)) 8))

    (test-exn "assign same variable"
              #rx"variables-error"
              (thunk (assign-variable vars "x" 'int))))

  (test-case "has-variable?"
    (test-not-false "has variable" (has-variable? vars "x"))
    (test-false "doesn't have variable" (has-variable? vars "y")))

  (test-case "find-variable"
    (test-equal? "variable find"
                 (find-variable vars "x")
                 x)

    (test-exn "variable not fund"
              #rx"variables-error"
              (thunk (find-variable vars "y"))))

  (test-case "variable-amount"
    (test-equal? "int variable"
                 (variable-amount vars)
                 8)

    (test-equal? "array variable"
                 (variable-amount
                  (assign-variable vars "y" 'array 8))
                 72)))
