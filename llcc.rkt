#lang racket

(require racket/cmdline)

(struct token (type val char-at))

(define (raise-error expr char-at msg)
  (displayln expr)
  (displayln (format "~a^" (make-string char-at #\space)))
  (displayln msg)
  (raise 'invalid-token))

(define (take-while lst f)
  (cond
    ((null? lst) (values '() '()))
    ((f (car lst))
     (let-values ([(take rest)
                   (take-while (cdr lst) f)])
       (values (cons (car lst) take) rest)))
    (else
     (values '() lst))))

(module+ test
  (require rackunit)
  (let-values ([(taken rest)
                (take-while (string->list "12 + 34") char-numeric?)])
    (check-equal? taken '(#\1 #\2)))

  (let-values ([(taken rest)
                (take-while (string->list "") char-numeric?)])
    (check-equal? taken '())))

(define (tokenize expr)
  (define (tokenize-rec lst char-at)
    (cond ((null? lst) '())
          ((equal? (car lst) #\space)
           (tokenize-rec (cdr lst) (+ 1 char-at)))
          ((equal? (car lst) #\+)
           (cons (token '+ #\+ char-at)
                 (tokenize-rec (cdr lst) (+ char-at))))
          ((equal? (car lst) #\-)
           (cons (token '- #\- char-at)
                 (tokenize-rec (cdr lst) (+ char-at))))
          ((char-numeric? (car lst))
           (define-values (taken rest) (take-while lst char-numeric?))
           (const (token 'number (list->string taken) (list->string lst))
                  (tokenize-rec rest (+ (length taken) char-at))))
          (else
           (raise-error "failed: invalid value" (car lst)))))
  (tokenize-rec (string->list expr) 0))


(define (parse expr)
  (define (parse-rec tokens)
    (cond ((null? tokens) '())
          (else
           (define token (car tokens))
           (match (token-type token)
             ['+
              (cons (format "\tadd rax, ~a" (token-val (cadr tokens)))
                    (parse-rec (cddr token)))]
             ['-
              (cons (format "\tsub rax, ~a" (token-val (cadr tokens)))
                    (parse-rec (cddr token)))]
             [else (raise-error expr (token-char-at token) "invalid token")]))))

  (define tokens (tokenize expr))
  (define first-token (car tokens))
  (unless (equal? (token-type first-token) 'number)
    (raise-error expr (token-char-at token) "invalid token"))

  (cons (format "\tmov rax,~a" (token-val first-token))
        (parse-rec (cdr tokens))))

(module+ test
  (check-equal?
   (parse (string->list "12")) '("\tmov rax, 12"))

  (check-equal?
   (parse (string->list "12+34")) '("\tmov rax, 12" "\tadd rax, 34"))
  (check-equal?
   (parse (string->list "12+34-12")) '("\tmov rax, 12" "\tadd rax, 34" "\tsub rax, 12")))

(define expr
  (command-line
   #:args (expr)
   expr))

(define (main)
  (displayln ".intel_syntax noprefix")
  (displayln ".global main")
  (displayln "main:")
  (displayln (string-join (parse expr) "\n"))
  (displayln "\tret"))

(main)
