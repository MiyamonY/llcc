;;;
;; File:  parser.rkt
;; Author: ymiyamoto
;;
;; Created on Fri Apr 12 17:51:56 2019
;;
#lang racket

(require  racket/contract)

(provide (contract-out
          [parse (string? . -> . (listof node?))])
         (struct-out node))

(struct node (type left right value msg)
  #:transparent)

(define (take-while ls f)
  (define (aux ls acc)
    (match ls
      [(list) (values (list->string (reverse acc)) '())]
      [(list a rest ...) #:when (f a) (aux rest (cons (car ls) acc))]
      [_ (values (list->string (reverse acc)) ls)]))
  (aux ls '()))

;; num : ("0" - "9")+
(define (num chars)
  (define-values (val rem) (take-while chars char-numeric?))

  (values (node 'NUM '() '() (string->number val) "") rem))

;;  var : "a"-"z"
(define (var chars)
  (define val (car chars))
  (define rem (cdr chars))

  (values (node 'VAR '() '() (string val) "") rem))

;; add  : mul add'
;; add' : \epsilon
;; add' : "+" mul add'
;; add' : "-" mul add'
(define (add chars)
  (define (add-sub nodes chars)
    (cond [(null? chars) (values nodes chars)]
          [(eq? (car chars) #\space) (add-sub nodes (cdr chars))]
          [(eq? (car chars) #\+)
           (let-values (([node1 rem] (mul (cdr chars))))
             (add-sub (node 'ADD nodes node1 #\+ "") rem))]
          [(eq? (car chars) #\-)
           (let-values (([node1 rem] (mul (cdr chars))))
             (add-sub (node 'ADD nodes node1 #\- "") rem))]
          [else (values nodes chars)]))

  (define-values (nodes rem) (mul chars))
  (add-sub nodes rem))

;; mul  : term mul'
;; mul' : \epsilon
;; mul' : "*" term mul'
;; mul' : "/" term mul'
(define (mul chars)
  (define (mul-sub nodes chars)
    (cond [(null? chars) (values nodes chars)]
          [(eq? (car chars) #\space) (mul-sub nodes (cdr chars))]
          [(eq? (car chars) #\*)
           (let-values (([term rem] (term (cdr chars))))
             (mul-sub (node 'MUL nodes term #\* "") rem))]
          [(eq? (car chars) #\/)
           (let-values (([term rem] (term (cdr chars))))
             (mul-sub (node 'MUL nodes term #\/ "") rem))]
          [else (values nodes chars)]))

  (define-values (nodes rem) (term chars))
  (mul-sub nodes rem))

;; term: num
;; term: "(" add ")"
(define (term chars)
  (cond
    [(null? chars) (error "invalid form")]
    [(eq? (car chars) #\space) (term (cdr chars))]
    [(eq? (car chars) #\()
     (let-values (([nodes rem] (add (cdr chars))))
       (cond [(null? rem) (error "reach eof")]
             [(eq? (car rem) #\)) (values nodes (cdr rem))]
             [else (error "parenes is not pair" nodes rem)]))]
    [(char-numeric? (car chars)) (num chars)]
    [(char-alphabetic? (car chars)) (var chars)]
    [else (error "parse error")]))

;; stmt: add ";"
(define (stmt chars)
  (define-values (node rem) (add chars))

  (cond
    [(null? rem) (error "reached eof")]
    [(eq? (car rem) #\;) (values node (cdr rem))]
    [else (error "statement is not end with ';'")]))

;; program : stmt program
;; program : \epsilon
(define (program chars)
  (define (aux nodes chars)
    (define-values (node rem) (stmt chars))
    (if (null? rem)
        (reverse (cons node nodes))
        (aux (cons node nodes) rem)))

  (aux '() chars))

(define (parse str)
  (program (string->list str)))

;; tests
(module+ test
  (require rackunit)

  (test-case "simple num"
    (let-values (([nodes rem] (num (string->list "123"))))
      (check-equal? nodes (node 'NUM '() '() 123 ""))
      (check-equal? rem '())))

  (test-case "parse single value"
    (check-equal? (parse "234; 1;")
                  (list (node 'NUM '() '() 234 "")
                        (node 'NUM '() '() 1 ""))))

  (test-case "parse valid arithmetic exp"
    (check-equal? (parse "(234 + 2)*3/ 2; 2+3;")
                  (list (node 'MUL
                              (node 'MUL
                                    (node 'ADD
                                          (node 'NUM '() '() 234 "")
                                          (node 'NUM '() '() 2 "")
                                          #\+
                                          "")
                                    (node 'NUM '() '() 3 "")
                                    #\*
                                    "")
                              (node 'NUM '() '() 2 "")
                              #\/
                              "")
                        (node 'ADD
                              (node 'NUM '() '() 2 "")
                              (node 'NUM '() '() 3 "")
                              #\+
                              ""))))

  (test-case "parse valid arithmetic exp"
    (check-equal? (parse "(234 + 2)/(3+3*2);")
                  (list (node 'MUL
                        (node 'ADD
                              (node 'NUM '() '() 234 "")
                              (node 'NUM '() '() 2 "")
                              #\+
                              "")
                        (node 'ADD
                              (node 'NUM '() '() 3 "")
                              (node 'MUL
                                    (node 'NUM '() '() 3 "")
                                    (node 'NUM '() '() 2 "")
                                    #\*
                                    "")
                              #\+
                              "")
                        #\/
                        ""))))

  (test-case "parse sinlge variable"
    (check-equal? (parse "2+c;")
                  (list (node 'ADD
                              (node 'NUM '() '() 2 "")
                              (node 'VAR '()' () "c" "")
                              #\+
                              ""))))

  (test-case "invalidate null exp"
    (check-exn exn:fail? (lambda () (parse ""))))

  (test-case "invalidate statement without ';'"
    (check-exn #rx"^reached eof$" (lambda () (parse "123*2"))))

  (test-case "invalidate double operator"
    (for-each (lambda (exp)
                (check-exn exn:fail? (lambda () (parse exp))))
              '("1 + + 2;"
                "1 * 2 + * 3;"
                "(1+2)**3;")))

  (test-case "parensis is not pair"
    (for-each (lambda (exp)
                (check-exn exn:fail? (lambda () (parse exp))))
              '("1 + (2 + 4;"
                "1 + (2 + 4));"
                "(1 + 2)) + 4;")))

  (test-case "invalidate parallel parenthis"
    (check-exn exn:fail? (lambda () (display (parse "(1+2) (3+4);")))))

  (test-case "invalidate exp with undefined token"
    (for-each (lambda (exp)
                (check-exn exn:fail? (lambda () (parse exp))))
              '("ab + 32;"
                ".+3;"
                "2+3&5+x;"))))
