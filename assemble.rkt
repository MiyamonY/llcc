;;;
;; File:  assemble.rkt
;; Author: ymiyamoto
;;
;; Created on Sun Apr 14 09:26:50 2019
;;
#lang racket
(require racket/contract)

(provide (contract-out
         [assemble (list? . -> . string?)]))

(define (assemble arith)
  (string-append
   ".intel_syntax noprefix\n"
   ".global main\n"
   "main:\n"
   (assemble-arith arith)
   "  ret\n"
   ))

(define (assemble-arith arith)
  (define (aux rest)
    (if (null? rest)
        ""
        (let ([operator (car rest)]
              [operand (cadr rest)])
          (string-append
           (cond
             [(equal? operator '+) (format "  add rax, ~v\n" operand)]
             [(equal? operator '-) (format "  sub rax, ~v\n" operand)]
             [else (error "invalid operator" operator)])
           (aux (cddr rest))))))

  (string-append
   (format "  mov rax, ~v\n" (car arith))
   (aux (cdr arith))))

(module+ test
  (require rackunit)

  (define (check-assemble? arith expect)
    (define actual (assemble arith))
    (check-equal? actual
                  (string-append ".intel_syntax noprefix\n"
                                 ".global main\n"
                                 "main:\n"
                                 expect
                                 "  ret\n"
                                 )))

  (define tests
    '(((5) . "  mov rax, 5\n")
      ((1 + 2 + 3) . "  mov rax, 1\n  add rax, 2\n  add rax, 3\n")
      ((1 - 2 + 3) . "  mov rax, 1\n  sub rax, 2\n  add rax, 3\n")))

  (for-each (lambda (c)
              (check-pred pair? c)
              (check-assemble? (car c) (cdr c)))
            tests))
