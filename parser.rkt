;;;
;; File:  parser.rkt
;; Author: ymiyamoto
;;
;; Created on Fri Apr 12 17:51:56 2019
;;
#lang racket

(require  racket/contract)

(provide (contract-out
          [parse (string? . -> . list?)])
         (struct-out node))

(struct node (type left right value msg)
  #:transparent)

(define (atoi c)
  (- (char->integer c) (char->integer #\0)))

(define (take-while ls f)
  (define (aux ls acc)
    (match ls
      [(list) (values (list->string (reverse acc)) '())]
      [(list a rest ...) #:when (f a) (aux rest (cons (car ls) acc))]
      [_ (values (list->string (reverse acc)) ls)]))
  (aux ls '()))

(define (num chars)
  (define-values (val rest) (take-while chars char-numeric?))
  (values (node 'NUM '() '() (string->number val) "") rest))

(define (add chars)
  (define-values (val rest) (mul chars))

  (define (add-sub nodes chars)
    (cond [(null? chars) (values nodes chars)]
          [(eq? (car chars) #\space) (add-sub nodes (cdr chars))]
          [(eq? (car chars) #\+)
           (let-values (([node1 rest1] (mul (cdr chars))))
             (add-sub (node 'ADD nodes node1 #\+ "") rest1))]
          [(eq? (car rest) #\-)
           (let-values (([node1 rest1] (mul (cdr chars))))
             (add-sub (node 'ADD nodes node1 #\- "") rest1))]
          [else (values nodes chars)]))
  (add-sub val rest))

(define (mul chars)
  (define-values (term1 rest) (term chars))

  (define (mul-sub nodes chars)
    (cond [(null? chars) (values nodes chars)]
          [(eq? (car chars) #\space) (mul-sub nodes (cdr chars))]
          [(eq? (car chars) #\*)
           (let-values (([term2 rest2] (term (cdr chars))))
             (mul-sub (node 'MUL nodes term2 #\* "") rest2))]
          [(eq? (car chars) #\/)
           (let-values (([term2 rest2] (term (cdr chars))))
             (mul-sub (node 'MUL nodes term2 #\/ "") rest2))]
          [else (values term1 chars)]))

  (mul-sub term1 rest))

(define (term chars)
  (cond
    [(null? chars) (error "invalid form")]
    [(eq? (car chars) #\space) (term (cdr chars))]
    [(eq? (car chars) #\()
     (let-values (([nodes rest] (add (cdr chars))))
       (cond [(null? rest) (error "reach eof")]
             [(eq? (car rest) #\)) (values nodes (cdr rest))]
             [else (error "parenes is not pair" nodes rest)]))]
    [(or (eq? (car chars) #\+ ) (eq? (car chars) #\-)) (error "unexpected operator")]
    [else (let-values (([node rest] (num chars)))
            (values node rest))]))

(define (parse str)
  (define-values (node rest) (mul (string->list str)))
  node)

(module+ test
  (require rackunit)

  (test-case "simple num"
    (let-values (([nodes rem] (num (string->list "123"))))
      (check-equal? nodes (node 'NUM '() '() 123 ""))
      (check-equal? rem '())))

  (test-case "parse single value"
    (check-equal? (parse "234")
                  (node 'NUM '() '() 234 "")))

  (test-case "parse arithmetic exp"
    (check-equal? (parse "(234 + 2)*3/ 2")
                  (node 'MUL
                        (node 'MUL
                              (node 'ADD
                                    (node 'NUM '() '() 234 "")
                                    (node 'NUM '() '() 2 "")
                                    #\+
                                    "")
                              (node 'NUM '() '() 3 "")
                              #\*
                              "")
                        (node 'NUM '() '() 2 "")
                        #\/
                        "")))

  (test-case "invalidate null exp"
    (check-exn exn:fail? (lambda () (parse ""))))

  (test-case "invalidate double operator"
    (check-exn exn:fail? (lambda () (displayln (parse "1 + + 2")))))

  (test-case "parensis is not pair"
    (check-exn exn:fail? (lambda () (parse "1 + (2 + 4"))))

  (test-case "invalidate parallel parenthis"
    (check-exn exn:fail? (lambda () (parse "(1+2) (3+4)"))))

  (test-case "invalidate exp with undefined token"
    (check-exn exn:fail? (lambda () (parse "a + 32")))))
