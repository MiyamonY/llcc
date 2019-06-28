#lang racket

(require racket/cmdline)

(struct token (type val char-at)
  #:transparent)

(define (token-null) null)

(define (token-number num char-at)
  (token 'number num char-at))

(define (token-operator op char-at)
  (token 'operator op char-at))

(define (token-plus char-at)
  (token-operator #\+ char-at))

(define (token-sub char-at)
  (token-operator #\- char-at))

(define (token-number? token)
  (equal? (token-type token) 'number))

(define (token-operator? token)
  (equal? (token-type token) 'operator))

(define (token-add? token)
  (and (token-operator? token) (equal? (token-val token) #\+)))

(define (token-sub? token)
  (and (token-operator? token) (equal? (token-val token) #\-)))

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
           (cons (token-operator (car lst) char-at)
                 (tokenize-rec (cdr lst) (add1 char-at))))
          ((char-numeric? (car lst))
           (define-values (taken rest) (take-while lst char-numeric?))
           (cons (token-number (string->number (list->string taken)) char-at)
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

(struct node (type left right val)
  #:transparent)

(define (node-number num)
  (node 'number null null num))

(define (node-operator left right op)
  (node 'operator left right op))

(define (node-plus left right)
  (node-operator left right #\+))

(define (node-minus left right)
  (node-operator left right #\-))

(define (node-number? node)
  (equal? (node-type node) 'number))

(define (node-operator? node)
  (equal? (node-type node) 'operator))

(define (parse input)
  ;; term = num
  (define (term tokens)
    (when (null? tokens)
      (parse-error input (string-length input) "expression ends unexpectedly"))

    (define token0 (car tokens))
    (unless (token-number? token0)
      (parse-error input (token-char-at token0) "token must be number"))
    (values (node 'number null null (token-val token0)) (cdr tokens)))

  ;; expr = term ("+" term | "-" term)*
  (define (expr tokens)
    (define (expr-rec term0 tokens)
      (if (null? tokens)
          term0
          (let ([operator (car tokens)])
            (unless (token-operator? operator)
              (parse-error input (token-char-at operator) "token must be operaotr"))
            (let-values ([(term1 rest) (term (cdr tokens))])
              (expr-rec (node-operator term0 term1 (token-val operator)) rest)))))

    (define-values (term0 rest) (term tokens))
    (expr-rec term0 rest))

  (expr (tokenize input)))

(module+ test
  (check-equal?
   (parse "12") (node-number 12))

  (check-equal?
   (parse "12+34")
   (node-plus (node-number 12) (node-number 34)))

  (check-equal?
   (parse " 12+ 34- 12")
   (node-minus (node-plus (node-number 12) (node-number 34))
               (node-number 12)))

  (check-exn #rx"parse-error" (lambda () (parse "12+")))
  (check-exn #rx"parse-error" (lambda () (parse "12+ +"))))

(define (push num)
  (format "\tpush ~a\n" num))

(define (add)
  (string-join '("\tpop rdi"
                 "\tpop rax"
                 "\tadd rax, rdi"
                 "\tpush rax")
               "\n"
               #:after-last "\n"))

(define (sub)
  (string-join '("\tpop rdi"
                 "\tpop rax"
                 "\tsub rax, rdi"
                 "\tpush rax")
               "\n"
               #:after-last "\n"))

(define (generate nodes)
  (define (generate-rec nodes)
    (if (null? nodes)
        ""
        (cond [(node-number? nodes) (push (node-val nodes))]
              [(node-operator? nodes)
               (string-append (generate-rec (node-left nodes))
                              (generate-rec (node-right nodes))
                              (case (node-val nodes)
                                [(#\+) (add)]
                                [(#\-) (sub)]))])))
  (string-append (generate-rec nodes) "\tpop rax\n"))

(define (main expr)
  (displayln ".intel_syntax noprefix")
  (displayln ".global main")
  (displayln "main:")
  (displayln  (generate (parse expr)))
  (displayln "\tret"))

(module+ main
  (define expr
    (command-line
     #:args (expr)
     expr))

  (main expr))
