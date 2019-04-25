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
          [assemble ((listof node?) . -> . string?)]))

(define ((add-indent [num 2]) str)
  (define indent (make-string num #\space))
  (format "~a~a" indent str))

(define (assemble nodes)
  (define commands
    (flatten (map (lambda (node)
           (append (assemble-arith node) '("  pop rax"))) nodes)))

  (string-join
   `(".intel_syntax noprefix"
     ".global main"
     "main:"
     ,@commands
     "  ret")
   "\n"))

(define (assemble-arith node)
  (define type (node-type node))
  (define value (node-value node))

  (match type
    ['NUM (list (format "  push ~v" value))]
    ['ADD (let ((left  (assemble-arith (node-left node)))
                (right (assemble-arith (node-right node))))
            (append left right
                    (map (add-indent 2)
                         `("pop rdi"
                           "pop rax"
                           ,(match value
                              [#\+ "add rax, rdi"]
                              [#\- "sub rax, rdi"]
                              [_  (error "invalid operator:" value)])
                           "push rax"))))]
    ['MUL (let ((left  (assemble-arith (node-left node)))
                (right (assemble-arith (node-right node))))
            (append left right
                    (map (add-indent 2)
                         `("pop rdi"
                           "pop rax"
                           ,@(match value
                               [#\* '("mul rdi")]
                               [#\/ '("mov rdx, 0" "div rdi")]
                               [_ (error "invalid operator:" value)])
                           "push rax"))))]))

(module+ test
  (require rackunit)

  (define (check-assemble? arith expect)
    (define actual (assemble arith))
    (check-equal? actual
                  (string-join
                   `(".intel_syntax noprefix"
                     ".global main"
                     "main:"
                     ,@(map (add-indent 2) expect)
                     "  ret")
                   "\n")))

  (define (add a b)
    (list (format "push ~v" a) (format "push ~v" b) "pop rdi" "pop rax" "add rax, rdi" "push rax"))

  (define (sub a b)
    (list (format "push ~v" a) (format "push ~v" b) "pop rdi" "pop rax" "sub rax, rdi" "push rax"))

  (test-case "assemble sinlge value"
    ;; 5; 2+3;
    (check-assemble? (list (node-num 5)
                           (node-add (node-num 2) (node-num 3)))
                     `("push 5" "pop rax" ,@(add 2 3) "pop rax")))

  (test-case "assmeble arith"
    ;; (2+3)/5*(3-10); 2+3;
    (check-assemble? (list (node-mul
                            (node-div
                             (node-add
                              (node-num 2)
                              (node-num 3))
                             (node-num 5))
                            (node-sub (node-num 3) (node-num 10)))
                           (node-add (node-num 2) (node-num 3)))
                     `(,@(add 2 3)
                       "push 5" "pop rdi" "pop rax" "mov rdx, 0" "div rdi" "push rax"
                       ,@(sub 3 10)
                       "pop rdi" "pop rax" "mul rdi" "push rax" "pop rax"
                       ,@(add 2 3)
                       "pop rax")))

  (test-case "invalidate invalid token"
    (for-each
     (lambda (input) (check-exn exn:fail? (lambda () (assemble input))))
     (list (list (node 'ADD
                       (node-num 3)
                       (node-num 5) #\space ""))
           (list (node 'MUL (node-num 1) (node-num 2) #\+ ""))))))
