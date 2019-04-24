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
         [assemble (node? . -> . string?)]))

(define ((add-indent [num 2]) str)
  (define indent (make-string num #\space))
  (format "~a~a" indent str))

(define (assemble nodes)
  (string-join
   (flatten  (list ".intel_syntax noprefix"
                   ".global main"
                   "main:"
                   (map (add-indent 2) (assemble-arith nodes))
                   "  pop rax"
                   "  ret"))
   "\n"))

(define (assemble-arith nodes)
  (define type (node-type nodes))
  (define value (node-value nodes))

  (match type
    ['NUM (list (format "push ~v" value))]
    ['ADD (let ((left  (assemble-arith (node-left nodes)))
                (right (assemble-arith (node-right nodes))))
            (append left right
                    (list "pop rdi" "pop rax"
                          (match value
                            [#\+ "add rax, rdi"]
                            [#\- "sub rax, rdi"]
                            [_  (error "invalid operator:" value)])
                          "push rax")))]
    ['MUL (let ((left  (assemble-arith (node-left nodes)))
                (right (assemble-arith (node-right nodes))))
            (append left right
                    (flatten (list "pop rdi" "pop rax"
                                   (match value
                                     [#\* "mul rdi"]
                                     [#\/ '("mov rdx, 0" "div rdi")]
                                     [_ (error "invalid operator:" value)])
                                   "push rax"))))]))

(module+ test
  (require rackunit)

  (define (check-assemble? arith expect)
    (define actual (assemble arith))
    (check-equal? actual
                  (string-join
                   (flatten (list ".intel_syntax noprefix"
                                  ".global main"
                                  "main:"
                                  (map (add-indent 2) expect)
                                  "  pop rax"
                                  "  ret"))
                   "\n")))

  (test-case "assemble sinlge value"
    (check-assemble? (node 'NUM '() '() 5 "") '("push 5")))

  (test-case "assmeble arith"
    ;; (2+3)/5*(3-10)
    (check-assemble? (node 'MUL
                           (node 'MUL
                                 (node 'ADD
                                       (node 'NUM '() '() 2 "")
                                       (node 'NUM '() '() 3 "")
                                       #\+
                                       "")
                                 (node 'NUM
                                       '()
                                       '()
                                       5
                                       "")
                                 #\/
                                 "")
                           (node 'ADD
                                 (node 'NUM '() '() 3 "")
                                 (node 'NUM '() '() 10 "")
                                 #\-
                                 "")
                           #\*
                           ""
                           )
                     '("push 2" "push 3" "pop rdi" "pop rax" "add rax, rdi" "push rax"
                                "push 5" "pop rdi" "pop rax" "mov rdx, 0" "div rdi" "push rax"
                                "push 3" "push 10" "pop rdi" "pop rax" "sub rax, rdi" "push rax"
                                "pop rdi" "pop rax" "mul rdi" "push rax")))

  (test-case "invalidate invalid token"
    (for-each
     (lambda (input) (check-exn exn:fail? (lambda () (assemble input))))
     (list (node 'ADD
                 (node 'NUM '() '() 3 "")
                 (node 'NUM '() '() 5 "") #\space "")
           (node 'MUL (node 'NUM '() '() 1 "") (node 'NUM '() '() 2 "") #\+ "")))))
