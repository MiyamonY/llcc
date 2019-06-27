#lang racket

(require racket/cmdline)

(struct token (type val char-at)
  #:transparent)

(define (token-number num char-at)
  (token 'number num char-at))
(define (token-plus char-at)
  (token 'operator #\+ char-at))
(define (token-minus char-at)
  (token 'operator #\- char-at))


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

(define (tokenize-error expr char-at msg)
  (raise-user-error
   'tokenize-error  "\n~a\n~a^\n~a\n" expr (make-string char-at #\space) msg))

(define (tokenize expr)
  (define (tokenize-rec lst char-at)
    (cond ((null? lst) '())
          ((equal? (car lst) #\space)
           (tokenize-rec (cdr lst) (add1 char-at)))
          ((or (equal? (car lst) #\+)
               (equal? (car lst) #\-))
           (cons (token 'operator (car lst) char-at)
                 (tokenize-rec (cdr lst) (add1 char-at))))
          ((char-numeric? (car lst))
           (define-values (taken rest) (take-while lst char-numeric?))
           (cons (token 'number (string->number (list->string taken)) char-at)
                 (tokenize-rec rest (+ (length taken) char-at))))
          (else
           (tokenize-error expr char-at "unexpected value"))))

  (tokenize-rec (string->list expr) 0))

(module+ test
  (check-equal? (tokenize "1+2")
                (list (token-number 1 0)
                      (token-plus 1)
                      (token-number 2 2))))

(define (parse-error expr char-at msg)
  (raise-user-error
   'parse-error  "\n~a\n~a^\n~a\n" expr (make-string char-at #\space) msg))

(define (parse expr)
  (define (parse-rec tokens)
    (cond ((null? tokens) '())
          (else
           (define top-token (car tokens))
           (match (token-type top-token)
             ['operator
              (define operator top-token)

              (when (null? (cdr tokens))
                (parse-error expr (token-char-at top-token) "expression ended unexpectedly"))
              (define operand (cadr tokens))
              (unless (equal? (token-type operand) 'number)
                (parse-error expr (token-char-at operand) "invalid token"))

              (define fmt (case (token-val operator)
                            [(#\+) "\tadd rax, ~a"]
                            [(#\-) "\tsub rax, ~a"]
                            [else (parse-error expr (token-char-at operand) "invalid operator")]))

              (cons (format fmt (token-val operand)) (parse-rec (cddr tokens)))]
             [else (parse-error expr (token-char-at top-token) "invalid token")]))))

  (define tokens (tokenize expr))
  (define first-token (car tokens))
  (unless (equal? (token-type first-token) 'number)
    (parse-error expr (token-char-at token) "invalid token"))

  (cons (format "\tmov rax, ~a" (token-val first-token))
        (parse-rec (cdr tokens))))

(module+ test
  (check-equal?
   (parse "12")
   '("\tmov rax, 12"))
  (check-equal?
   (parse "12+34")
   '("\tmov rax, 12" "\tadd rax, 34"))
  (check-equal?
   (parse " 12+ 34- 12")
   '("\tmov rax, 12" "\tadd rax, 34" "\tsub rax, 12"))

  (check-exn #rx"parse-error" (lambda () (parse "12+")))
  (check-exn #rx"parse-error" (lambda () (parse "12+ +"))))

(define (main expr)
  (define orders (parse expr))
  (displayln ".intel_syntax noprefix")
  (displayln ".global main")
  (displayln "main:")
  (displayln (string-join orders "\n"))
  (displayln "\tret"))

(module+ main
  (define expr
    (command-line
     #:args (expr)
     expr))

  (main expr))
