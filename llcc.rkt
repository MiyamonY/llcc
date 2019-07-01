#lang racket

(require racket/cmdline)

(struct token (char-at)
  #:transparent)

(struct token-number token (num)
  #:transparent)

(struct token-return token ()
  #:transparent)

(struct token-stmt token ()
  #:transparent)

(struct token-identifier token (name)
  #:transparent)

(struct token-if token ()
  #:transparent)

(struct token-else token ()
  #:transparent)

(struct token-paren token (val)
  #:transparent)

(define (token-lparen char-at)
  (token-paren char-at "("))

(define (token-rparen char-at)
  (token-paren char-at ")"))

(define (token-lparen? token)
  (and (token-paren? token) (equal? (token-paren-val token) "(")))

(define (token-rparen? token)
  (and (token-paren? token) (equal? (token-paren-val token) ")")))

(struct token-operator token (op)
  #:transparent)

(define (token-plus char-at)
  (token-operator char-at "+"))

(define (token-sub char-at)
  (token-operator char-at "-"))

(define (token-eq char-at)
  (token-operator char-at "=="))

(define (token-neq char-at)
  (token-operator char-at "!="))

(define (token-lt char-at)
  (token-operator char-at "<" ))

(define (token-le char-at)
  (token-operator char-at "<="))

(define (token-gt char-at)
  (token-operator char-at ">"))

(define (token-ge char-at)
  (token-operator char-at ">="))

(define (token-assign char-at)
  (token-operator char-at "="))

(define (token-plus? token)
  (and (token-operator? token) (equal? (token-operator-op token) "+")))

(define (token-add? token)
  (token-plus? token))

(define (token-minus? token)
  (and (token-operator? token) (equal? (token-operator-op token) "-")))

(define (token-sub? token)
  (token-minus? token))

(define (token-mul? token)
  (and (token-operator? token) (equal? (token-operator-op token) "*")))

(define (token-div? token)
  (and (token-operator? token) (equal? (token-operator-op token) "/")))

(define (token-eq? token)
  (and (token-operator? token) (equal? (token-operator-op token) "==")))

(define (token-neq? token)
  (and (token-operator? token) (equal? (token-operator-op token) "!=")))

(define (token-lt? token)
  (and (token-operator? token) (equal? (token-operator-op token) "<")))

(define (token-le? token)
  (and (token-operator? token) (equal? (token-operator-op token) "<=")))

(define (token-gt? token)
  (and (token-operator? token) (equal? (token-operator-op token) ">")))

(define (token-ge? token)
  (and (token-operator? token) (equal? (token-operator-op token) ">=")))

(define (token-assign? token)
  (and (token-operator? token) (equal? (token-operator-op token) "=")))

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
           (cons (token-operator char-at (string (peek lst)))
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
           (cons (token-number char-at (string->number (list->string taken)))
                 (tokenize-rec remaining (+ (length taken) char-at)))]
          [(char-lower-case? (peek lst))
           (define-values (taken remaining)
             (take-while lst (lambda (c) (or (char-numeric? c) (char-alphabetic? c)))))
           (define keyword (list->string taken))
           (define token
             (match keyword
               ["return" (token-return char-at)]
               ["if" (token-if char-at)]
               [ "else" (token-else char-at)]
               [_ (token-identifier char-at keyword)]))
           (cons token (tokenize-rec remaining (+ (length taken) char-at)))]
          (else
           (tokenize-error expr char-at "unexpected value"))))

  (tokenize-rec (string->list expr) 0))

(module+ test
  (check-equal? (tokenize "1+2")
                (list (token-number 0 1)
                      (token-plus 1)
                      (token-number 2 2)))

  (check-equal? (tokenize "a+b;a=1;")
                (list (token-identifier 0 "a")
                      (token-plus 1)
                      (token-identifier 2 "b")
                      (token-stmt 3)
                      (token-identifier 4 "a")
                      (token-assign 5)
                      (token-number 6 1)
                      (token-stmt 7))))

(define (parse-error expr char-at msg)
  (raise-user-error
   'parse-error  "\n~a\n~a^\n~a\n" expr (make-string char-at #\space) msg))

(struct node ()
  #:transparent)

(struct node-number node (val)
  #:transparent)

(struct node-assign node (left right)
  #:transparent)

(struct node-local-variable node (name offset)
  #:transparent)

(struct node-return node (expr)
  #:transparent)

(struct node-if node (conditional true-clause false-clause)
  #:transparent)

(struct node-operator node (op left right)
  #:transparent)

(define (node-plus left right)
  (node-operator "+" left right))

(define (node-sub left right)
  (node-operator "-" left right ))

(define (node-eq left right)
  (node-operator "==" left right))

(define (node-neq left right)
  (node-operator "!=" left right))

(define (node-lt left right)
  (node-operator "<" left right))

(define (node-le left right)
  (node-operator "<=" left right))

(define (node-add? node)
  (and (node-operator? node) (equal? (node-operator-op node) "+")))

(define (node-sub? node)
  (and (node-operator? node) (equal? (node-operator-op node) "-")))

(define (node-mul? node)
  (and (node-operator? node) (equal? (node-operator-op node) "*")))

(define (node-div? node)
  (and (node-operator? node) (equal? (node-operator-op node) "/")))

(define (node-eq? node)
  (and (node-operator? node) (equal? (node-operator-op node) "==")))

(define (node-neq? node)
  (and (node-operator? node) (equal? (node-operator-op node) "!=")))

(define (node-lt? node)
  (and (node-operator? node) (equal? (node-operator-op node) "<")))

(define (node-le? node)
  (and (node-operator? node) (equal? (node-operator-op node) "<=")))

(define (reverse-compare op)
  (cond [(equal? op ">") "<"]
        [(equal? op "<") ">"]
        [(equal? op "<=") ">="]
        [(equal? op ">=") "<="]))

(define (token-must-be token-pred tokens input)
  (cond [(empty? tokens)
         (parse-error input (string-length input) "assign ends unexpectedly")]
        [(not (token-pred (first tokens)))
         (parse-error input (token-char-at (first tokens))
                      (format "wrong token: ~a" (object-name token-pred)))]))

(define (parse input)
  (define variable-offsets (make-hash))

  (define new-offset
    ((lambda ()
       (define offset 0)
       (lambda ()
         (set! offset (+ offset 8))
         offset))))

  ;; term = num | ident | "(" expr ")""
  (define (term tokens)
    (when (null? tokens)
      (parse-error input (string-length input) "expression ends unexpectedly"))

    (define token0 (car tokens))
    (cond [(token-number? token0)
           (values (node-number (token-number-num token0)) (cdr tokens))]
          [(token-identifier? token0)
           (define name (token-identifier-name token0))
           (define offset
             (or (hash-ref variable-offsets name #f)
                 (let ([offset (new-offset)])
                   (hash-set! variable-offsets name offset)
                   offset)))
           (values (node-local-variable name offset) (cdr tokens))]
          [(token-lparen? token0)
           (define-values (expr0 remaining) (expr (cdr tokens)))
           (token-must-be token-rparen? remaining input)
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
             (define-values (unary1 remaining) (unary (cdr tokens)))
             (mul-rec (node-operator (token-operator-op (car tokens)) unary0 unary1) remaining)]
            [else (values unary0 tokens)]))
    (call-with-values (lambda () (unary tokens)) mul-rec))

  ;; add = mul ("+" mul | "-" mul)*
  (define (add tokens)
    (define (add-rec mul0 tokens)
      (cond [(null? tokens) (values mul0 tokens)]
            [(or (token-add? (first tokens)) (token-sub? (first tokens)))
             (define operator (first tokens))
             (define-values (mul1 remaining) (mul (rest tokens)))
             (add-rec (node-operator (token-operator-op operator) mul0 mul1) remaining)]
            [else (values mul0 tokens)]))

    (call-with-values (lambda () (mul tokens)) add-rec))

  ;; relational = add ("<" add | "<=" add | ">" add | ">=" add)*
  (define (relational tokens)
    (define (relational-rec add0 tokens)
      (cond [(empty? tokens) (values add0 tokens)]
            [(or (token-lt? (first tokens)) (token-le? (first tokens)))
             (define operator (first tokens))
             (define-values (add1 remaining) (add (rest tokens)))
             (relational-rec (node-operator (token-operator-op operator) add0 add1) remaining)]
            [(or (token-gt? (first tokens)) (token-ge? (first tokens)))
             (define operator (first tokens))
             (define-values (add1 remaining) (add (rest tokens)))
             (relational-rec (node-operator
                              (reverse-compare (token-operator-op operator)) add1 add0) remaining)]
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

  ;; expr = assign*
  (define (expr tokens)
    (assign tokens))

  ;; stmt = expr ";"
  ;;      | "return" expr ";"
  ;;      | "if" "(" expr ")" stmt ("else" stmt)?
  (define (stmt tokens)
    (define-values (node remaining)
      (cond [(empty? tokens)
             (parse-error input (string-length input) "stmt is empty")]
            [(token-return? (first tokens))
             (define-values (expr0 remaining) (expr (rest tokens)))
             (token-must-be token-stmt? remaining input)
             (values (node-return expr0) (rest remaining))]
            [(token-if? (first tokens))
             (token-must-be token-lparen? (rest tokens) input)
             (define-values (conditional remaining0) (expr (rest (rest tokens))))
             (token-must-be token-rparen? remaining0 input)
             (define-values (true-clause remaining1) (stmt (rest remaining0)))
             (cond [(token-else? (first remaining1))
                    (define-values (false-clause remaining2) (stmt (rest remaining1)))
                    (values (node-if conditional true-clause false-clause)  remaining2)]
                   [else
                    (values (node-if conditional true-clause null) remaining1)])]
            [else
             (define-values (expr0 remaining) (expr tokens))
             (token-must-be token-stmt? remaining input)
             (values expr0 (rest remaining))]))

    (values node remaining))

  ;; program = (stmt)*
  (define (program tokens)
    (define (program-rec tokens)
      (cond [(null? tokens) '()]
            [else
             (define-values (stmt0 remaining) (stmt tokens))
             (cons stmt0 (program-rec remaining))]))
    (program-rec tokens))

  (program (tokenize input)))

(module+ test
  (check-equal?
   (parse "12;") (list (node-number 12)))

  (check-equal?
   (parse "12+34;")
   (list (node-plus (node-number 12) (node-number 34))))

  (check-equal?
   (parse " -12+ 34- 12;")
   (list (node-sub (node-plus (node-sub (node-number 0) (node-number 12)) (node-number 34))
                   (node-number 12))))

  (check-equal?
   (parse "x=y=1;")
   (list (node-assign (node-local-variable "x" 8)
                      (node-assign (node-local-variable "y" 16)
                                   (node-number 1)))))

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

(define (generate-error msg)
  (raise-user-error
   'generate-error "~a\n" msg))

(define (push num)
  `(,(format "push ~a" num)))

(define (reserve-local-variables)
  '("push rbp"
    "mov rbp, rsp"
    "sub rsp, 208"))

(define (free-local-variables)
  '("mov rsp, rbp"
    "pop rbp"))

(define (push-result)
  '("push rax"))

(define (pop-result)
  '("pop rax"))

(define (pop-operands)
  '("pop rdi"
    "pop rax"))

(define (return)
  '("ret"))

(define (generate-add)
  '("add rax, rdi"))

(define (generate-sub)
  '("sub rax, rdi"))

(define (generate-mul)
  '("imul rax, rdi"))

(define (generate-div)
  '("cqo"
    "div rdi"))

(define (generate-eq)
  '("cmp rax, rdi"
    "sete al"
    "movzb rax, al"))

(define (generate-neq)
  '("cmp rax, rdi"
    "setne al"
    "movzb rax, al"))

(define (generate-lt)
  '("cmp rax, rdi"
    "setl al"
    "movzb rax, al"))

(define (generate-le)
  '("cmp rax, rdi"
    "setle al"
    "movzb rax, al"))

(define (generate-load-from-local-variable)
  '("pop rax"
    "mov rax, [rax]"
    "push rax"))

(define (generate-store-to-local-variable)
  '("pop rdi"
    "pop rax"
    "mov [rax],rdi"
    "push rdi"))

(define (generate-left-value node)
  (unless (node-local-variable? node)
    (generate-error (format "left value must be local variable: ~a" (object-name node))))

  `("mov rax, rbp"
    ,(format "sub rax, ~a" (node-local-variable-offset node))
    "push rax"))

(define (generate nodes)
  (define gen-label
    ((lambda ()
       (define label-num 0)
       (lambda ()
         (set! label-num (add1 label-num))
         (format ".Llabel~a" label-num)))))

  (define (generate-rec node)
    (if (null? node)
        ""
        (cond [(node-number? node) (push (node-number-val node))]
              [(node-local-variable? node)
               `(,@(generate-left-value node)
                 ,@(generate-load-from-local-variable))]
              [(node-assign? node)
               `(,@(generate-left-value (node-assign-left node))
                 ,@(generate-rec (node-assign-right node))
                 ,@(generate-store-to-local-variable))]
              [(node-return? node)
               `(,@(generate-rec (node-return-expr node))
                 ,@(pop-result)
                 ,@(free-local-variables)
                 ,@(return))]
              [(node-if? node)
               (define label-else (gen-label))
               (define label-end (gen-label))
               `(,@(generate-rec (node-if-conditional node))
                 ,@(list "pop rax"
                         "cmp rax, 0"
                         (format "je ~a" label-else))
                 ,@(generate-rec (node-if-true-clause node))
                 ,(format "jmp ~a" label-end)
                 ,(format "~a:" label-else)
                 ,@(generate-rec (node-if-false-clause node))
                 ,(format "~a:" label-end))]
              [(node-operator? node)
               `(,@(generate-rec (node-operator-left node))
                 ,@(generate-rec (node-operator-right node))
                 ,@(pop-operands)
                 ,@(cond [(node-add? node) (generate-add)]
                         [(node-sub? node) (generate-sub)]
                         [(node-mul? node) (generate-mul)]
                         [(node-div? node) (generate-div)]
                         [(node-eq? node) (generate-eq)]
                         [(node-neq? node) (generate-neq)]
                         [(node-lt? node) (generate-lt)]
                         [(node-le? node) (generate-le)]
                         [else
                          (generate-error (format "unexpected operator: ~a" (node-operator-op node)))])
                 ,@(push-result))])))

  (string-join
   `(".intel_syntax noprefix"
     ".global main"
     "main:"
     ,(string-join
       (flatten
        (list
         (reserve-local-variables)
         (map
          (lambda (node) (append (generate-rec node) (pop-result)))
          nodes)
         (free-local-variables)
         (return)))
       "\n\t"
       #:before-first "\t"))
   "\n"))

(define (compile expr)
  (displayln (generate (parse expr))))

(module+ main
  (define expr
    (command-line
     #:args (expr)
     expr))

  (compile expr))
