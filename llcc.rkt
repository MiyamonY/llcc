#lang racket

(require racket/cmdline)

(define expr
  (command-line
   #:args (expr)
   expr))

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


(define (parse lst)
  (cond ((null? lst) '())
        ((eq? (car lst) #\+)
         (define-values (taken rest) (take-while (cdr lst) char-numeric?))
         (cons (format "\tadd rax, ~a" (list->string taken)) (parse rest)))
        ((eq? (car lst) #\-)
         (define-values (taken rest) (take-while (cdr lst) char-numeric?))
         (cons (format "\tsub rax, ~a" (list->string taken)) (parse rest)))
        ((char-numeric? (car lst))
         (define-values (taken rest) (take-while lst char-numeric?))
         (cons (format "\tmov rax, ~a" (list->string taken)) (parse rest)))
        (else (error "failed: invalid value" (car lst)))))

(module+ test
  (check-equal?
   (parse (string->list "12")) '("\tmov rax, 12"))

  (check-equal?
   (parse (string->list "12+34")) '("\tmov rax, 12" "\tadd rax, 34"))
  (check-equal?
   (parse (string->list "12+34-12")) '("\tmov rax, 12" "\tadd rax, 34" "\tsub rax, 12")))

(define (main)
  (displayln ".intel_syntax noprefix")
  (displayln ".global main")
  (displayln "main:")
  (displayln (string-join (parse (string->list expr)) "\n"))
  (displayln "\tret"))

(main)
