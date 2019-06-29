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

(define (token-lparen char-at)
  (token 'lparen #\( char-at))

(define (token-rparen char-at)
  (token 'rparen #\) char-at))

(define (token-number? token)
  (equal? (token-type token) 'number))

(define (token-operator? token)
  (equal? (token-type token) 'operator))

(define (token-plus? token)
  (and (token-operator? token) (equal? (token-val token) #\+)))

(define (token-add? token)
  (token-plus? token))

(define (token-minus? token)
  (and (token-operator? token) (equal? (token-val token) #\-)))

(define (token-sub? token)
  (token-minus? token))

(define (token-mul? token)
  (and (token-operator? token) (equal? (token-val token) #\*)))

(define (token-div? token)
  (and (token-operator? token) (equal? (token-val token) #\/)))

(define (token-lparen? token)
  (equal? (token-type token) 'lparen))

(define (token-rparen? token)
  (equal? (token-type token) 'rparen))

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
    (cond [(null? lst) '()]
          [(equal? (car lst) #\space)
           (tokenize-rec (cdr lst) (add1 char-at))]
          [(member (car lst) '(#\+ #\- #\* #\/))
           (cons (token-operator (car lst) char-at)
                 (tokenize-rec (cdr lst) (add1 char-at)))]
          [(equal? (car lst) #\()
           (cons (token-lparen char-at) (tokenize-rec (cdr lst) (add1 char-at)))]
          [(equal? (car lst) #\))
           (cons (token-rparen char-at) (tokenize-rec (cdr lst) (add1 char-at)))]
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

(define (node-sub left right)
  (node-operator left right #\-))

(define (node-number? node)
  (equal? (node-type node) 'number))

(define (node-operator? node)
  (equal? (node-type node) 'operator))

(define (parse input)
  ;; term = num | "(" expr ")""
  (define (term tokens)
    (when (null? tokens)
      (parse-error input (string-length input) "expression ends unexpectedly"))

    (define token0 (car tokens))
    (cond [(token-number? token0)
           (values (node-number (token-val token0)) (cdr tokens))]
          [(token-lparen? token0)
           (define-values (expr0 remaining) (expr (cdr tokens)))
           (unless (token-rparen? (car remaining))
             (parse-error input (token-char-at (car remaining)) "paren is not closed"))
           (values expr0 (cdr remaining))]
          [else
           (parse-error input (token-char-at token0) "token must be number or (")]))

  ;; unary = ("+" | "-")? term
  (define (unary tokens)
    (when (null? tokens)
      (parse-error input (string-length input) "expression ends unexpectedly"))

    (define unary-operator (car tokens))
    (cond [(token-plus? unary-operator)
           (term (cdr tokens))]
          [(token-minus? unary-operator)
           (define-values (term0 remaining) (term (cdr tokens)))
           (values (node-sub (node-number 0) term0) remaining)]
          [else (term tokens)]))

  ;; mul = unary ("*" unary | "/" unary)*
  (define (mul tokens)
    (define (mul-rec unary0 tokens)
      (cond [(null? tokens) (values unary0 tokens)]
            [(or (token-mul? (car tokens)) (token-div? (car tokens)))
             (let-values ([(unary1 remaining) (unary (cdr tokens))])
               (mul-rec (node-operator unary0 unary1 (token-val (car tokens))) remaining))]
            [else (values unary0 tokens)]))
    (call-with-values (lambda () (unary tokens)) mul-rec))

  ;; expr = mul ("+" mul | "-" mul)*
  (define (expr tokens)
    (define (expr-rec mul0 tokens)
      (cond [(null? tokens) (values mul0 tokens)]
            [(or (token-add? (car tokens)) (token-sub? (car tokens)))
             (let-values ([(operator) (car tokens)]
                          [(mul1 remaining) (mul (cdr tokens))])
               (expr-rec (node-operator mul0 mul1 (token-val operator)) remaining))]
            [else (values mul0 tokens)]))

    (call-with-values (lambda () (mul tokens)) expr-rec))

  (define-values (nodes remaining) (expr (tokenize input)))

  (unless (null? remaining)
    (parse-error input (token-char-at (car remaining)) "unused token"))
  nodes)

(module+ test
  (check-equal?
   (parse "12") (node-number 12))

  (check-equal?
   (parse "12+34")
   (node-plus (node-number 12) (node-number 34)))

  (check-equal?
   (parse " -12+ 34- 12")
   (node-sub (node-plus (node-sub (node-number 0) (node-number 12)) (node-number 34))
             (node-number 12)))


  (check-exn #rx"parse-error" (lambda () (parse "12+")))
  (check-exn #rx"parse-error" (lambda () (parse "12+ +")))
  (check-exn #rx"parse-error" (lambda () (parse " 12+ 34 12"))))

(define (push num)
  (format "\tpush ~a\n" num))

(define (push-result)
  "\tpush rax\n")

(define (pop-operands)
  (string-join '("pop rdi"
                 "pop rax")
               "\n\t"
               #:before-first "\t"
               #:after-last "\n"))

(define (pop-result)
  "\tpop rax\n")

(define (add)
  "\tadd rax, rdi\n")

(define (sub)
  "\tsub rax, rdi\n")

(define (mul)
  "\timul rax, rdi\n")

(define (div)
  (string-join '("cqo"
                 "div rdi")
               "\n\t"
               #:before-first "\t"
               #:after-last "\n"))

(define (generate-error msg)
  (raise-user-error
   'generate-error "~a\n" msg))

(define (generate nodes)
  (define (generate-rec nodes)
    (if (null? nodes)
        ""
        (cond [(node-number? nodes) (push (node-val nodes))]
              [(node-operator? nodes)
               (string-append (generate-rec (node-left nodes))
                              (generate-rec (node-right nodes))
                              (pop-operands)
                              (case (node-val nodes)
                                [(#\+) (add)]
                                [(#\-) (sub)]
                                [(#\*) (mul)]
                                [(#\/) (div)]
                                [else
                                 (generate-error (format "unepexted operator: ~a" (node-val nodes)))])
                              (push-result))])))

  (string-append
   ".intel_syntax noprefix\n"
   ".global main\n"
   "main:\n"
   (generate-rec nodes)
   (pop-result)
   "\tret"))

(define (compile expr)
  (displayln (generate (parse expr))))

(module+ main
  (define expr
    (command-line
     #:args (expr)
     expr))

  (compile expr))
