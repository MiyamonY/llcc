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

(define (parse-arith str)
  (define (converter s)
    (match s
      ["+" '+]
      ["-" '-]
      [_ (string->number s)]
      ))

  (map converter (string-split str)))

(module+ test
  (require rackunit)

  (check-equal? (parse-arith "1") (list 1))
  (check-equal? (parse-arith "1 + 23 + 324") (list 1 '+ 23 '+ 324))
  (check-equal? (parse-arith "234 + 2 - 3") (list 234 '+ 2 '- 3)))
