#lang racket

(require racket/cmdline)

(struct token (type val char-at)
  #:transparent)

(define (token-null) null)

(define (token-number num char-at)
  (token 'number num char-at))

(define (token-identifier c char-at)
  (token 'identifier c char-at))

(define (token-stmt char-at)
  (token 'statement #\; char-at))

(define (token-operator op char-at)
  (token 'operator op char-at))

(define (token-plus char-at)
  (token-operator #\+ char-at))

(define (token-sub char-at)
  (token-operator #\- char-at))

(define (token-lparen char-at)
  (token-operator #\( char-at))

(define (token-rparen char-at)
  (token-operator #\) char-at))

(define (token-eq char-at)
  (token-operator "==" char-at))

(define (token-neq char-at)
  (token-operator "!=" char-at))

(define (token-lt char-at)
  (token-operator #\< char-at))

(define (token-le char-at)
  (token-operator "<=" char-at))

(define (token-gt char-at)
  (token-operator #\> char-at))

(define (token-ge char-at)
  (token-operator ">=" char-at))

(define (token-assign char-at)
  (token-operator #\= char-at))

(define (token-number? token)
  (equal? (token-type token) 'number))

(define (token-stmt? token)
  (equal? (token-type token) 'statement))

(define (token-identifier? token)
  (equal? (token-type token) 'identifier))

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
  (and (token-operator? token) (equal? (token-val token) #\()))

(define (token-rparen? token)
  (and (token-operator? token) (equal? (token-val token) #\))))

(define (token-eq? token)
  (and (token-operator? token) (equal? (token-val token) "==")))

(define (token-neq? token)
  (and (token-operator? token) (equal? (token-val token) "!=")))

(define (token-lt? token)
  (and (token-operator? token) (equal? (token-val token) #\<)))

(define (token-le? token)
  (and (token-operator? token) (equal? (token-val token) "<=")))

(define (token-gt? token)
  (and (token-operator? token) (equal? (token-val token) #\>)))

(define (token-ge? token)
  (and (token-operator? token) (equal? (token-val token) ">=")))

(define (token-assign? token)
  (and (token-operator? token) (equal? (token-val token) #\=)))

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

(define (peek lst)
  (if (empty? lst)
      #f
      (first lst)))

(define (tokenize expr)
  (define (tokenize-rec lst char-at)
    (cond [(not (peek lst)) '()]
          [(equal? (peek lst) #\space)
           (tokenize-rec (rest lst) (add1 char-at))]
          [(member (peek lst) '(#\+ #\- #\* #\/))
           (cons (token-operator (peek lst) char-at)
                 (tokenize-rec (rest lst) (add1 char-at)))]
          [(equal? (peek lst) #\()
           (cons (token-lparen char-at) (tokenize-rec (rest lst) (add1 char-at)))]
          [(equal? (peek lst) #\))
           (cons (token-rparen char-at) (tokenize-rec (rest lst) (add1 char-at)))]
          [(equal? (peek lst) #\=)
           (define next (peek (rest lst)))
           (cond [(equal? next #\=)
                  (cons (token-eq char-at) (tokenize-rec (cddr lst) (+ 2 char-at)))]
                 [else
                  (cons (token-assign char-at) (tokenize-rec (rest lst) (add1 char-at)))])]
          [(equal? (peek lst) #\!)
           (define next (peek (rest lst)))
           (cond [(equal? next #\=)
                  (cons (token-neq char-at) (tokenize-rec (cddr lst) (+ 2 char-at)))]
                 [else
                  (tokenize-error expr (add1 char-at) "unexpected value")])]
          [(equal? (peek lst) #\<)
           (define next (peek (rest lst)))
           (cond [(equal? next #\=)
                  (cons (token-le char-at) (tokenize-rec (rest (rest lst)) (+ char-at 2)))]
                 [else
                  (cons (token-lt char-at) (tokenize-rec (rest lst) (add1 char-at)))])]
          [(equal? (peek lst) #\>)
           (define next (peek (rest lst)))
           (cond [(equal? next #\=)
                  (cons (token-ge char-at) (tokenize-rec (rest (rest lst)) (+ char-at 2)))]
                 [else
                  (cons (token-gt char-at) (tokenize-rec (rest lst) (add1 char-at)))
                  ])]
          [(equal? (peek lst) #\;)
           (cons (token-stmt char-at) (tokenize-rec (rest lst) (add1 char-at)))]
          [(char-numeric? (peek lst))
           (define-values (taken remaining) (take-while lst char-numeric?))
           (cons (token-number (string->number (list->string taken)) char-at)
                 (tokenize-rec remaining (+ (length taken) char-at)))]
          [(char-lower-case? (peek lst))
           (cons (token-identifier (peek lst) char-at)
                 (tokenize-rec (rest lst) (add1 char-at)))]
          (else
           (tokenize-error expr char-at "unexpected value"))))

  (tokenize-rec (string->list expr) 0))

(module+ test
  (check-equal? (tokenize "1+2")
                (list (token-number 1 0)
                      (token-plus 1)
                      (token-number 2 2)))

  (check-equal? (tokenize "a+b;a=1;")
                (list (token-identifier #\a 0)
                      (token-plus 1)
                      (token-identifier #\b 2)
                      (token-stmt 3)
                      (token-identifier #\a 4)
                      (token-assign 5)
                      (token-number 1 6)
                      (token-stmt 7))))

(define (parse-error expr char-at msg)
  (raise-user-error
   'parse-error  "\n~a\n~a^\n~a\n" expr (make-string char-at #\space) msg))

(struct node (type left right val [offset #:auto])
  #:auto-value 0
  #:transparent)

(define (node-number num)
  (node 'number null null num))

(define (node-assign left right)
  (node 'assign left right #\=))

(define (variable-offset c)
  (* (add1 (- (char->integer c) (char->integer #\a))) 8))

(module+ test
  (check-equal? (variable-offset #\a) 8)
  (check-equal? (variable-offset #\c) 24))

(define (node-local-variable c)
  (node 'local-variable null null (variable-offset c)))

(define (node-operator left right op)
  (node 'operator left right op))

(define (node-plus left right)
  (node-operator left right #\+))

(define (node-sub left right)
  (node-operator left right #\-))

(define (node-eq left right)
  (node-operator left right "=="))

(define (node-neq left right)
  (node-operator left right "!="))

(define (node-lt left right)
  (node-operator left right #\<))

(define (node-le left right)
  (node-operator left right "<="))

(define (node-number? node)
  (equal? (node-type node) 'number))

(define (node-local-variable? node)
  (equal? (node-type node) 'local-variable))

(define (node-assign? node)
  (equal? (node-type node) 'assign))

(define (node-operator? node)
  (equal? (node-type node) 'operator))

(define (node-add? node)
  (and (node-operator? node) (equal? (node-val node) #\+)))

(define (node-sub? node)
  (and (node-operator? node) (equal? (node-val node) #\-)))

(define (node-mul? node)
  (and (node-operator? node) (equal? (node-val node) #\*)))

(define (node-div? node)
  (and (node-operator? node) (equal? (node-val node) #\/)))

(define (node-eq? node)
  (and (node-operator? node) (equal? (node-val node) "==")))

(define (node-neq? node)
  (and (node-operator? node) (equal? (node-val node) "!=")))

(define (node-lt? node)
  (and (node-operator? node) (equal? (node-val node) #\<)))

(define (node-le? node)
  (and (node-operator? node) (equal? (node-val node) "<=")))

(define (reverse-compare op)
  (cond [(equal? op #\>) #\<]
        [(equal? op #\<) #\>]
        [(equal? op "<=") ">="]
        [(equal? op ">=") "<="]))

(define (parse input)
  ;; term = num | ident "(" expr ")""
  (define (term tokens)
    (when (null? tokens)
      (parse-error input (string-length input) "expression ends unexpectedly"))

    (define token0 (car tokens))
    (cond [(token-number? token0)
           (values (node-number (token-val token0)) (cdr tokens))]
          [(token-identifier? token0)
           (values (node-local-variable (token-val token0)) (cdr tokens))]
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

  ;; add = mul ("+" mul | "-" mul)*
  (define (add tokens)
    (define (add-rec mul0 tokens)
      (cond [(null? tokens) (values mul0 tokens)]
            [(or (token-add? (first tokens)) (token-sub? (first tokens)))
             (let-values ([(operator) (first tokens)]
                          [(mul1 remaining) (mul (rest tokens))])
               (add-rec (node-operator mul0 mul1 (token-val operator)) remaining))]
            [else (values mul0 tokens)]))

    (call-with-values (lambda () (mul tokens)) add-rec))

  ;; relational = add ("<" add | "<=" add | ">" add | ">=" add)*
  (define (relational tokens)
    (define (relational-rec add0 tokens)
      (cond [(empty? tokens) (values add0 tokens)]
            [(or (token-lt? (first tokens)) (token-le? (first tokens)))
             (let-values ([(operator) (first tokens)]
                          [(add1 remaining) (add (rest tokens))])
               (relational-rec (node-operator add0 add1 (token-val operator)) remaining))]
            [(or (token-gt? (first tokens)) (token-ge? (first tokens)))
             (let-values ([(operator) (first tokens)]
                          [(add1 remaining) (add (rest tokens))])
               (relational-rec (node-operator add1 add0 (reverse-compare (token-val operator))) remaining))]
            [else (values add0 tokens)]))

    (call-with-values (lambda () (add tokens)) relational-rec))

  ;; equality = relational ("==" relational | "!=" relational)*
  (define (equality tokens)
    (define (equality-rec rel0 tokens)
      (cond [(empty? tokens) (values rel0 tokens)]
            [(token-eq? (first tokens))
             (define-values (rel1 remaining) (relational (rest tokens)))
             (equality-rec (node-eq rel0 rel1) remaining)]
            [(token-neq? (first tokens))
             (define-values (rel1 remaining) (relational (rest tokens)))
             (equality-rec (node-neq rel0 rel1) remaining)]
            [else (values rel0 tokens)]))

    (call-with-values (lambda () (relational tokens)) equality-rec))

  ;; assign = equality ("=" assing)?
  (define (assign tokens)
    (define (assign-aux equ0 tokens)
      (when (empty? tokens)
        (parse-error input (string-length input) "assign ends unexpectedly"))

      (cond [(token-assign? (first tokens))
             (define-values (assign0 remaining) (assign (rest tokens)))
             (values (node-assign equ0 assign0) remaining)]
            [else
             (values equ0 tokens)]))

    (call-with-values (lambda () (equality tokens)) assign-aux))

  ;; expr = assign
  (define (expr tokens)
    (assign tokens))

  ;; stmt = expr ";"
  (define (stmt tokens)
    (define-values (expr0 remaining) (expr tokens))

    (when (empty? remaining)
      (parse-error input (token-char-at (string-length input)) "statement ends unexpectedly"))

    (unless (token-stmt? (first remaining))
      (parse-error input (token-char-at (first remaining)) "statement is not end with ;"))

    (values expr0 (rest remaining)))

  ;; program = stmt
  (define (program tokens)
    (stmt tokens))

  (define-values (nodes remaining) (program (tokenize input)))

  (unless (null? remaining)
    (parse-error input (token-char-at (car remaining)) "unused token"))
  nodes)

(module+ test
  (check-equal?
   (parse "12;") (node-number 12))

  (check-equal?
   (parse "12+34;")
   (node-plus (node-number 12) (node-number 34)))

  (check-equal?
   (parse " -12+ 34- 12;")
   (node-sub (node-plus (node-sub (node-number 0) (node-number 12)) (node-number 34))
             (node-number 12)))

  (check-equal?
   (parse "x=y=1;")
   (node-assign (node-local-variable #\x) (node-assign (node-local-variable #\y) (node-number 1))) )

  (define input-for-exn
    '("12+;"
      "12+ +;"
      " 12+ 34 12;"
      " 12+ 34"
      " 12+ 34=;"
      " 12+ 34<;"))

  (for-each (lambda (input)
              (check-exn #rx"parse-error" (lambda () (parse input))))
            input-for-exn))

(define (push num)
  (format "\tpush ~a\n" num))

(define (reserve-stack)
  (string-join '("push rbp"
                 "mov rbp, rsp"
                 "sub rsp, 208")
               "\n\t"
               #:before-first "\t"
               #:after-last "\n"))

(define (reset-stack)
  (string-join '("mov rsp, rbp"
                 "pop rbp")
               "\n\t"
               #:before-first "\t"
               #:after-last "\n"))
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

(define (generate-add)
  "\tadd rax, rdi\n")

(define (generate-sub)
  "\tsub rax, rdi\n")

(define (generate-mul)
  "\timul rax, rdi\n")

(define (generate-div)
  (string-join '("cqo"
                 "div rdi")
               "\n\t"
               #:before-first "\t"
               #:after-last "\n"))

(define (generate-eq)
  (string-join '("cmp rax, rdi"
                 "sete al"
                 "movzb rax, al")
               "\n\t"
               #:before-first "\t"
               #:after-last "\n"))

(define (generate-neq)
  (string-join '("cmp rax, rdi"
                 "setne al"
                 "movzb rax, al")
               "\n\t"
               #:before-first "\t"
               #:after-last "\n"))

(define (generate-lt)
  (string-join '("cmp rax, rdi"
                 "setl al"
                 "movzb rax, al")
               "\n\t"
               #:before-first "\t"
               #:after-last "\n"))

(define (generate-le)
  (string-join '("cmp rax, rdi"
                 "setle al"
                 "movzb rax, al")
               "\n\t"
               #:before-first "\t"
               #:after-last "\n"))

(define (generate-error msg)
  (raise-user-error
   'generate-error "~a\n" msg))

(define (generate-left-value node)
  (unless (node-local-variable? node)
    (generate-error (format "left value must be local variable: ~a" (node-val node))))

  (string-join (list
                "mov rax, rbp"
                (format "sub rax, ~a" (node-offset node))
                "push rax")
               "\n\t"
               #:before-first "\t"
               #:after-last "\n"))

(define (generate node)
  (define (generate-rec node)
    (if (null? node)
        ""
        (cond [(node-number? node) (push (node-val node))]
              [(node-local-variable? node)
               (string-append (generate-left-value node)
                              (string-join '("pop rax"
                                             "mov rax, [rax]"
                                             "push rax")
                                           "\n\t"
                                           #:before-first "\t"
                                           #:after-last "\n"))]
              [(node-assign? node)
               (string-append (generate-left-value (node-left node))
                              (generate-rec (node-right node))
                              (string-join '("pop rdi"
                                             "pop rax"
                                             "mov [rax],rdi"
                                             "push rdi")
                                           "\n\t"
                                           #:before-first "\t"
                                           #:after-last "\n"))]
              [(node-operator? node)
               (string-append
                (generate-rec (node-left node))
                (generate-rec (node-right node))
                (pop-operands)
                (cond [(node-add? node) (generate-add)]
                      [(node-sub? node) (generate-sub)]
                      [(node-mul? node) (generate-mul)]
                      [(node-div? node) (generate-div)]
                      [(node-eq? node) (generate-eq)]
                      [(node-neq? node) (generate-neq)]
                      [(node-lt? node) (generate-lt)]
                      [(node-le? node) (generate-le)]
                      [else
                       (generate-error (format "unepexted operator: ~a" (node-val node)))])
                (push-result))])))

  (string-append
   ".intel_syntax noprefix\n"
   ".global main\n"
   "main:\n"
   (reserve-stack)
   (generate-rec node)
   (pop-result)
   (reset-stack)
   "\tret"))

(define (compile expr)
  (displayln (generate (parse expr))))

(module+ main
  (define expr
    (command-line
     #:args (expr)
     expr))

  (compile expr))
