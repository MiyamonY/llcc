;;;
;; File:  assemble.rkt
;; Author: ymiyamoto
;;
;; Created on Sun Apr 14 09:26:50 2019
;;
#lang racket
(require racket/contract)
(require "parser.rkt")

(provide (contract-out
         [assemble (list? . -> . string?)]))

(define (assemble tokens)
  (string-join
   (list ".intel_syntax noprefix"
         ".global main"
         "main:"
         (assemble-arith tokens)
         "  ret")
   "\n"))

(define (assemble-arith tokens)
  (define (aux tokens)
    (cond
      [(eq? (token-type (car tokens)) 'EOF) '()]
      [(eq? (token-type (car tokens)) '+)
       (cons (format "  add rax, ~v" (token-val (cadr tokens))) (aux (cddr tokens)))]
      [(eq? (token-type (car tokens)) '-)
       (cons (format "  sub rax, ~v" (token-val (cadr tokens))) (aux (cddr tokens)))]
      [else (error 'assembe-error "assemble invalid token")]))

  (define first-token (car tokens))
  (define rest-token (cdr tokens))

  (if (not (eq? (token-type first-token) 'NUM))
      (error 'assemble-error "first token is not number")
      (string-join (cons
                    (format "  mov rax, ~v" (token-val first-token))
                    (aux rest-token))
                   "\n")))

(module+ test
  (require rackunit)

  (define (check-assemble? arith expect)
    (define actual (assemble arith))
    (check-equal? actual
                  (string-join
                   (list ".intel_syntax noprefix"
                         ".global main"
                         "main:"
                         expect
                         "  ret")
                   "\n")))

  (test-case "parse sinlge value"
    (check-assemble? (list (token 'NUM 5 "") (token 'EOF "" "")) "  mov rax, 5"))

  (test-case "parse arith"
    (check-assemble? (list (token 'NUM 1 "")
                           (token '+ #\+ "")
                           (token 'NUM 2 "")
                           (token '- #\- "")
                           (token 'NUM 3 "")
                           (token 'EOF "" ""))
                     (string-join
                      (list "  mov rax, 1" "  add rax, 2" "  sub rax, 3") "\n")))

  (test-case "invalidate double operator"
    (check-exn exn:fail? (lambda () (assemble (list (token 'NUM 1 "")
                                                    (token '+ #\+ "")
                                                    (token '+ #\+ "")
                                                    (token 'NUM 2 "")
                                                    (token 'EOF "" ""))))))
  (test-case "invalidate no operands"
    (check-exn exn:fail? (lambda () (assemble (list (token 'NUM 1 "")
                                                    (token '+ #\+ "")
                                                    (token 'EOF "" "")))))))
