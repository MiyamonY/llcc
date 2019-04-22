;;;
;; File:  parser.rkt
;; Author: ymiyamoto
;;
;; Created on Fri Apr 12 17:51:56 2019
;;
#lang racket

(require  racket/contract)

(provide (contract-out
          [parse-arith (string? . -> . list?)])
         (struct-out token))

(struct token (type val msg)
  #:transparent)

(define (atoi c)
  (- (char->integer c) (char->integer #\0)))

(module+ test
  (require rackunit)
  (check-equal? (match '()
                  [(list) 'null]
                  [_ 'not-null ]
                  ) 'null)
  (check-equal? (match (list 1 2 3)
                  [(list a rest ...) rest]) (list 2 3)))

(define (take-while ls f)
  (define (aux ls acc)
    (match ls
      [(list) (values (string->number (list->string (reverse acc))) '())]
      [(list a rest ...) #:when (f a) (aux rest (cons (car ls) acc))]
      [_ (values (string->number (list->string (reverse acc))) ls)] ))
  (aux ls '()))

(define (tokenize chars)
  (cond
      [(null? chars) (list (token 'EOF "" ""))]
      [(eq? (car chars) #\space) (tokenize (cdr chars))]
      [(eq? (car chars) #\+) (cons (token '+ #\+ "") (tokenize (cdr chars)))]
      [(eq? (car chars) #\-) (cons (token '- #\- "") (tokenize (cdr chars)))]
      [(char-numeric? (car chars)) (let-values
                                       ([(num rest) (take-while chars char-numeric?)])
                                     (cons (token 'NUM num "") (tokenize rest)))]
      [else (error 'parse-error "undefined token")]))

(define (parse-arith str)
  (tokenize (string->list str)))

(module+ test
  (require rackunit)

  (check-equal? (parse-arith "1")
                (list (token 'NUM 1 "") (token 'EOF "" "")))

  (check-equal? (parse-arith "1 + 23 + 324")
                (list (token 'NUM 1 "")
                      (token '+ #\+ "")
                      (token 'NUM 23 "")
                      (token '+ #\+ "")
                      (token 'NUM 324 "")
                      (token 'EOF "" "")))

  (check-equal? (parse-arith "234+2 - 3")
                (list (token 'NUM 234 "")
                      (token '+ #\+ "")
                      (token 'NUM 2 "")
                      (token '- #\- "")
                      (token 'NUM 3 "")
                      (token 'EOF "" "")))

  (check-exn exn:fail? (lambda () (parse-arith "a + 32"))))
