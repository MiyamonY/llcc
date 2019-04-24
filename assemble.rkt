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

(define (add-indent str)
  (format "  ~a" str))

(define (assemble nodes)
  (string-join
   (list ".intel_syntax noprefix"
         ".global main"
         "main:"
         (string-join (map add-indent (assemble-arith nodes)) "\n")
         "  pop rax"
         "  ret")
   "\n"))

(define (assemble-arith nodes)
  (define type (node-type nodes))
  (define value (node-value nodes))

  (cond
    [(eq? type 'NUM) (list (format "push ~a" value))]
    [(and (eq? type 'ADD) (eq? value #\+))
          (let ((left (assemble-arith (node-left nodes)))
                (right (assemble-arith (node-right nodes))))
            (append left right '("pop rdi" "pop rax" "add rax, rdi" "push rax")))]
    [(and (eq? type 'ADD) (eq? value #\-))
          (let ((left (assemble-arith (node-left nodes)))
                (right (assemble-arith (node-right nodes))))
            (append left right '("pop rdi" "pop rax" "sub rax, rdi" "push rax")))]
    [(and (eq? type 'MUL) (eq? value #\*))
          (let ((left (assemble-arith (node-left nodes)))
                (right (assemble-arith (node-right nodes))))
            (append left right '("pop rdi" "pop rax" "mul rdi" "push rax")))]
    [(and (eq? type 'MUL) (eq? value #\/))
          (let ((left (assemble-arith (node-left nodes)))
                (right (assemble-arith (node-right nodes))))
            (append left right '("pop rdi" "pop rax" "div rdi" "push rax")))]
    [else (error "undefined node" type value)]))

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
                         "  pop rax"
                         "  ret")
                   "\n")))

  (test-case "assemble sinlge value"
    (check-assemble? (node 'NUM '() '() 5 "")
                     (string-join (map add-indent (list "push 5")) "\n")))

  (test-case "assmelbe valid arith"
    ;; (3+2)*4/(2-5)
    (check-assemble? (node 'MUL
                           (node 'MUL
                                 (node 'ADD
                                       (node 'NUM '() '() 3 "")
                                       (node 'NUM '() '() 2 "")
                                       #\+
                                       "")
                                 (node 'NUM '() '() 4 "")
                                 #\*
                                 "")
                           (node 'ADD
                                 (node 'NUM '() '() 2 "")
                                 (node 'NUM '() '() 5 "")
                                 #\-
                                 "")
                           #\/
                           "")
                     (string-join
                      (map add-indent
                           (list "push 3" "push 2" "pop rdi" "pop rax"
                                 "add rax, rdi" "push rax"
                                 "push 4" "pop rdi" "pop rax" "mul rdi" "push rax"
                                 "push 2" "push 5" "pop rdi" "pop rax"
                                 "sub rax, rdi" "push rax"
                                 "pop rdi" "pop rax" "div rdi" "push rax")) "\n")))

  (test-case "invalidate invalid node"
    (check-exn exn:fail? (lambda () (assemble (node 'MUL
                                                    (node 'NUM '() '() 3 "")
                                                    (node 'NUM '() '() 5 "")
                                                    #\+ ""))))))
