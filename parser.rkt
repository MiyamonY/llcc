;;;
;; File:  parser.rkt
;; Author: ymiyamoto
;;
;; Created on Fri Apr 12 17:51:56 2019
;;
#lang racket

(require  racket/contract)

(provide (contract-out
          [parse-arith (string? . -> . list?)]))

(struct token (type val msg)
  #:transparent)

(define (atoi c)
  (- (char->intger c) (char->integer #\0)))

(define (parse-num chars)
  (cond
    ((null? chars) 0)
    ((char-numeric? (car chars))
     (parse-num cdr)
     (+ (* (atoi (car chars)) 10) ))
    (else ()))

(define (tokenize chars)
  (cond
      ((null? chars) (type 'EOF "" ""))
      ((eq? (car chars) #\space) (tokenize (cdr chars)))
      ((eq? (car chars) #\+) (cons (token '+ #\+ "") (tokenize (cdr chars))))
      ((eq? (car chars) #\-) (cons (token '+ #\- "") (tokenize (cdr chars))))
      (else (cons (token 'NUM 1 "") (tokenize (cdr chars))))))

(define (parse-arith str)
  (tokenize (string->list str)))

(module+ test
  (require rackunit)

  (check-equal? (parse-arith "1") (list (token type-int 1 "")))
  (check-equal? (parse-arith "1 + 23 + 324") (list 1 '+ 23 '+ 324))
  (check-equal? (parse-arith "234 + 2 - 3") (list 234 '+ 2 '- 3)))
