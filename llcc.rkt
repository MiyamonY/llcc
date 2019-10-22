#lang racket

(require racket/cmdline)

(struct token (char-at)
  #:transparent)

(struct token-number token (num)
  #:transparent)

(struct token-return token ()
  #:transparent)

(struct token-comma token ()
  #:transparent)

(struct token-semicolon token ()
  #:transparent)

(struct token-identifier token (name)
  #:transparent)

(struct token-if token ()
  #:transparent)

(struct token-else token ()
  #:transparent)

(struct token-while token ()
  #:transparent)

(struct token-for token ()
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

(struct token-curly-brace token (val)
  #:transparent)

(define (token-lcurly-brace char-at)
  (token-curly-brace char-at "{"))

(define (token-rcurly-brace char-at)
  (token-curly-brace char-at "}"))

(define (token-lcurly-brace? token)
  (and (token-curly-brace? token) (equal? (token-curly-brace-val token) "{")))

(define (token-rcurly-brace? token)
  (and (token-curly-brace? token) (equal? (token-curly-brace-val token) "}")))

(struct token-operator token (op)
  #:transparent)

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
          [(equal? (peek lst) #\,)
           (cons (token-comma char-at) (tokenize-rec (rest lst) (add1 char-at)) )]
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
           (cons (token-semicolon char-at) (tokenize-rec (rest lst) (add1 char-at)))]
          [(equal? (peek lst) #\{)
           (cons (token-lcurly-brace char-at) (tokenize-rec (rest lst) (add1 char-at)))]
          [(equal? (peek lst) #\})
           (cons (token-rcurly-brace char-at) (tokenize-rec (rest lst) (add1 char-at)))]
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
               ["else" (token-else char-at)]
               ["while" (token-while char-at)]
               ["for" (token-for char-at)]
               [_ (token-identifier char-at keyword)]))
           (cons token (tokenize-rec remaining (+ (length taken) char-at)))]
          (else
           (tokenize-error expr char-at "unexpected value"))))

  (tokenize-rec (string->list expr) 0))

(module+ test
  (test-exn "invalid operator "#rx"tokenize-error" (lambda () (tokenize "1 !! 2")))
  (check-exn #rx"tokenize-error" (lambda () (tokenize "~"))))

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

(struct node-while node (conditional body)
  #:transparent)

(struct node-for node (init conditional next body)
  #:transparent)

(struct node-block node (bodys)
  #:transparent)

(struct node-func-call node (func args)
  #:transparent)

(struct node-func-declaration node (name args body variables)
  #:transparent)

(struct node-operator node (op left right)
  #:transparent)

(struct variable (name offset)
  #:transparent)

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
  (define variables (make-hash))

  (define new-offset
    ((lambda ()
       (define offset 0)
       (lambda ()
         (set! offset (+ offset 8))
         offset))))

  ;; term = num | ident ("(" (expr ("," expr)*) ? ")")? | "(" expr ")""
  (define (term tokens)
    (when (null? tokens)
      (parse-error input (string-length input) "expression ends unexpectedly"))

    (cond [(token-number? (first tokens))
           (values (node-number (token-number-num (first tokens))) (rest tokens))]
          [(token-identifier? (first tokens))
           (define name (token-identifier-name (first tokens)))
           (cond [(token-lparen? (cadr tokens))
                  (define remaining (cddr tokens))
                  (cond [(token-rparen? (car remaining))
                         (values (node-func-call name '()) (cdr remaining))]
                        [else
                         (define (args-aux exprs tokens)
                           (if (not (token-comma? (first tokens)))
                               (values (reverse exprs) tokens)
                               (let-values ([(expr0 remaining) (expr (cdr tokens))])
                                 (args-aux (cons expr0 exprs)  remaining))))
                         (define-values (expr0 remaining1) (expr remaining))
                         (define-values (args remaining2) (args-aux (list expr0) remaining1))
                         (token-must-be token-rparen? remaining2 input)
                         (values (node-func-call name args) (cdr remaining2))])]
                 [else
                  (define offset
                    (if (hash-has-key? variables name)
                        (variable-offset (hash-ref variables name))
                        (let ([offset (new-offset)])
                          (hash-set! variables name (variable name offset))
                          offset)))
                  (values (node-local-variable name offset) (cdr tokens))])]
          [(token-lparen? (first tokens))
           (define-values (expr0 remaining) (expr (rest tokens)))
           (token-must-be token-rparen? remaining input)
           (values expr0 (cdr remaining))]
          [else
           (parse-error input (token-char-at (first tokens)) "token must be number or ( or identifier")]))

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
  ;;      | "while" "(" expr ")" stmt
  ;;      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
  ;;      | "{" stmt* "}"
  (define (stmt tokens)
    (define-values (node remaining)
      (cond [(empty? tokens)
             (parse-error input (string-length input) "stmt is empty")]
            [(token-return? (first tokens))
             (define-values (expr0 remaining) (expr (rest tokens)))
             (token-must-be token-semicolon? remaining input)
             (values (node-return expr0) (rest remaining))]
            [(token-if? (first tokens))
             (token-must-be token-lparen? (rest tokens) input)
             (define-values (conditional remaining0) (expr (rest (rest tokens))))
             (token-must-be token-rparen? remaining0 input)
             (define-values (true-clause remaining1) (stmt (rest remaining0)))
             (cond [(null? remaining1)
                    (values (node-if conditional true-clause null) remaining1)]
                   [(token-else? (first remaining1))
                    (define-values (false-clause remaining2) (stmt (rest remaining1)))
                    (values (node-if conditional true-clause false-clause)  remaining2)]
                   [else
                    (values (node-if conditional true-clause null) remaining1)])]
            [(token-while? (first tokens))
             (token-must-be token-lparen? (rest tokens) input)
             (define-values (conditional remaining) (expr (rest (rest tokens))))
             (token-must-be token-rparen? remaining input)
             (define-values (body remaining0) (stmt (rest remaining)))
             (values (node-while conditional body) remaining0)]
            [(token-for? (first tokens))
             (token-must-be token-lparen? (rest tokens) input)
             (define-values (init remaining0)
               (if (token-semicolon? (caddr tokens))
                   (values null (cddr tokens))
                   (expr (cddr tokens))))
             (token-must-be token-semicolon? remaining0 input)

             (define-values (conditional remaining1)
               (if (token-semicolon? (cadr remaining0))
                   (values null (rest remaining0))
                   (expr (rest remaining0))))
             (token-must-be token-semicolon? remaining1 input)

             (define-values (next remaining2)
               (if (token-rparen? (cadr remaining1))
                   (values null (rest remaining1))
                   (expr (rest remaining1))))
             (token-must-be token-rparen? remaining2 input)

             (define-values (body remaining3) (stmt (rest remaining2)))
             (values (node-for init conditional next body) remaining3)]
            [(token-lcurly-brace? (first tokens))
             (define (blocks-rec stmts tokens)
               (cond [(null? tokens)
                      (parse-error input (string-length input) "block ends unexpctedly")]
                     [(token-rcurly-brace? (first tokens))
                      (values (reverse stmts) (rest tokens))]
                     [else
                      (define-values (stmt0 remaining) (stmt tokens))
                      (blocks-rec (cons stmt0 stmts) remaining)]))

             (define-values (stmts remaining0) (blocks-rec '() (rest tokens)))
             (values (node-block stmts) remaining0)]
            [else
             (define-values (expr0 remaining) (expr tokens))
             (token-must-be token-semicolon? remaining input)
             (values expr0 (rest remaining))]))

    (values node remaining))

  (define (star a terminate?)
    (lambda (tokens)
      (define (rec nodes tokens)
        (cond [(null? tokens) (values (reverse nodes) tokens)]
              [(terminate? (first tokens)) (values (reverse nodes) tokens)]
              [else
               (define-values (node remaining) (a tokens))
               (rec (cons node nodes) remaining)]))
      (rec '() tokens)))

  ;; declaration = ("ident" "(" ")" "{" (stmt)* "}")*
  (define (declaration tokens)
    (define (decl tokens)
      (cond [(null? tokens) (values '() '())]
            [(token-identifier? (first tokens))
             (define name (token-identifier-name (first tokens)))
             (token-must-be token-lparen? (rest tokens) input)
             (token-must-be token-rparen? (drop tokens 2) input)
             (token-must-be token-lcurly-brace? (drop tokens 3) input)
             (set! variables (make-hash))
             (define-values (stmts remaining0) ((star stmt token-rcurly-brace?) (drop tokens 4)))
             (token-must-be token-rcurly-brace? remaining0 input)
             (values (node-func-declaration name '() stmts variables) (rest remaining0))]
            [else (parse-error input (token-char-at (first tokens)) "unexpected token")]))

    ((star decl (compose1 not token-identifier?)) tokens))

  ;; program = decraration
  (define (program tokens)
    (define-values (decl remaining) (declaration tokens))
    (when (not (null? remaining))
      (parse-error input (token-char-at (first tokens)) "There exists unconsumed tokens"))
    decl)

  (define tokens (tokenize input))
  (values (program tokens) variables))

(module+ test
  (define (node-add left right)
    (node-operator "+" left right))

  (define (node-mul left right)
    (node-operator "*" left right))

  (define (node-div left right)
    (node-operator "/" left right))

  (define (parse-node input)
    (define-values (node _) (parse input))
    node)

  (define (node-main body variables)
    (node-func-declaration "main" '() body variables))

  (test-equal? "num"
               (parse-node "main( ) {return 12;}")
               (list
                (node-main (list (node-return (node-number 12)))
                           (make-hash))))

  (test-equal? "identifier" (parse-node "main() {x;}")
               (list
                (node-main (list (node-local-variable "x" 8))
                           (make-hash `(("x" . ,(variable "x" 8)))))))

  (test-equal? "func call without args" (parse-node "main(){test();}")
               (list
                (node-main (list (node-func-call "test" '()))
                           (make-hash))))

  (test-equal? "func call with one arg" (parse-node "main(){test(1);}")
               (list
                (node-main (list (node-func-call "test" (list (node-number 1))))
                           (make-hash))))

  (test-equal? "func call with max args" (parse-node "main(){test(1,2,3,4,5,6);}")
               (list
                (node-main
                 (list (node-func-call "test" (list (node-number 1)
                                                    (node-number 2)
                                                    (node-number 3)
                                                    (node-number 4)
                                                    (node-number 5)
                                                    (node-number 6))))
                 (make-hash))))

  (test-equal? "parens" (parse-node "main(){(1+2);}")
               (list (node-main (list (node-add (node-number 1) (node-number 2)))
                                (make-hash))))

  (test-equal? "unary plus" (parse-node "main(){+1;}")
               (list (node-main (list (node-number 1))
                                (make-hash))))

  (test-equal? "unary minus" (parse-node "main(){-3;}")
               (list (node-main (list (node-sub (node-number 0) (node-number 3)))
                                (make-hash))))

  (test-equal? "mul muls and divs" (parse-node "main(){2*3/2*3/2;}")
               (list (node-main
                      (list
                       (node-div
                        (node-mul
                         (node-div
                          (node-mul
                           (node-number 2) (node-number 3))
                          (node-number 2)) (node-number 3)) (node-number 2)))
                      (make-hash))))

  (test-equal? "relationals" (parse-node "main(){1<2<=3>=2>1;}")
               (list (node-main
                      (list
                       (node-lt (node-number 1)
                                (node-le (node-number 2)
                                         (node-le
                                          (node-lt (node-number 1) (node-number 2)) (node-number 3)))))
                      (make-hash))))

  (test-equal? "euqalities" (parse-node "main(){1==2 != 3;}")
               (list (node-main (list (node-neq (node-eq (node-number 1) (node-number 2)) (node-number 3)))
                                (make-hash))))

  (test-equal? "assign" (parse-node "main(){x=y=1;}")
               (list (node-main
                      (list (node-assign (node-local-variable "x" 8)
                                         (node-assign (node-local-variable "y" 16) (node-number 1))))
                      (make-hash `(("x" . ,(variable "x" 8)) ("y" . ,(variable "y" 16)))))))

  (test-equal? "return"
               (parse-node "main(){return 3;}")
               (list (node-main
                      (list (node-return (node-number 3)))
                      (make-hash))))

  (test-equal? "ifs"
               (parse-node "main(){if (1 <= 2 ) return 3;if (1 < 2) return 3; else return 4;if(1) 1;}")
               (list (node-main
                      (list
                       (node-if (node-le (node-number 1) (node-number 2))
                                (node-return (node-number 3)) null)
                       (node-if (node-lt (node-number 1) (node-number 2))
                                (node-return (node-number 3)) (node-return (node-number 4)))
                       (node-if (node-number 1) (node-number 1) null))
                      (make-hash))))

  (test-equal? "whiles"
               (parse-node "main(){while (x<3) return 4;}")
               (list (node-main
                      (list (node-while (node-lt (node-local-variable "x" 8) (node-number 3))
                                        (node-return (node-number 4))))
                      (make-hash `(("x" . ,(variable "x" 8)))))))

  (test-equal? "for all"
               (parse-node "main(){for (x=0; x < 10; x = x + 1) 2 + 3;}")
               (list (node-main
                      (list (node-for (node-assign (node-local-variable "x" 8) (node-number 0))
                                      (node-lt (node-local-variable "x" 8) (node-number 10))
                                      (node-assign (node-local-variable "x" 8)
                                                   (node-add
                                                    (node-local-variable "x" 8)
                                                    (node-number 1)))
                                      (node-add (node-number 2) (node-number 3))))
                      (make-hash `(("x" . ,(variable "x" 8)))))))

  (test-equal? "for while"
               (parse-node "main(){for (;;) 1;}")
               (list (node-main
                      (list
                       (node-for null
                                 null
                                 null
                                 (node-number 1)))
                      (make-hash))))

  (test-equal? "blocks"
               (parse-node "main(){{a=1; b=2; c=a+b;return c;}}")
               (list (node-main
                      (list
                       (node-block (list
                                    (node-assign (node-local-variable "a" 8) (node-number 1))
                                    (node-assign (node-local-variable "b" 16) (node-number 2))
                                    (node-assign (node-local-variable "c" 24)
                                                 (node-add (node-local-variable "a" 8)
                                                           (node-local-variable "b" 16)))
                                    (node-return (node-local-variable "c" 24)))))
                      (make-hash
                       `(("a" . ,(variable "a" 8)) ("b" . ,(variable "b" 16)) ("c" . ,(variable "c" 24)))))))

  (test-equal? "function declarations"
               (parse-node "a(){return 1+2;} b(){3*4;} main(){return a()+b();}")
               (list
                (node-func-declaration
                 "a"
                 '()
                 (list (node-return (node-operator "+" (node-number 1) (node-number 2))))
                 (make-hash))
                (node-func-declaration
                 "b"
                 '()
                 (list (node-operator "*" (node-number 3) (node-number 4)))
                 (make-hash))
                (node-func-declaration
                 "main"
                 '()
                 (list
                  (node-return
                   (node-operator "+" (node-func-call "a" '()) (node-func-call "b" '()))))
                 (make-hash))))

  (define input-for-exn
    '("main(){12+;}"
      "main(){12+ +;}"
      "main(){12+ -;}"
      "main(){;}"
      "main(){ 12+ 34 12;}"
      "main(){ 12+ 34}"
      "main(){ 12+ 34=;}"
      "main(){ 12+ 34<;}"
      "main(){if(1+2) 3 else return 4;}"
      "main(){if(1 return 3; else retunr 4;}"
      "main(){if(1+2) 3; else }"
      "main(){if(1+2); }"
      "main(){if(1+2}"
      "main(){3/}"
      "main(){3/-}"
      "main(){{1+2;}"
      "main(){"
      "main){}"
      "main({}"
      "(){}"))

  (for-each (lambda (input)
              (test-exn "invalid inputs" #rx"parse-error" (lambda () (parse-node input))))
            input-for-exn))

(struct instruction ()
  #:transparent)

(struct instruction-command instruction (command)
  #:transparent)

(struct instruction-label instruction (label)
  #:transparent)

(define (generate-error msg)
  (raise-user-error
   'generate-error "~a\n" msg))

(define (push num)
  (list (instruction-command (format "push ~a" num))))

(define (reserve-local-variables variables)
  (list (instruction-command "push rbp")
        (instruction-command "mov rbp, rsp")
        (instruction-command (format "sub rsp, ~a" (* 8 (hash-count variables))))))

(define (free-local-variables)
  (list (instruction-command "mov rsp, rbp")
        (instruction-command "pop rbp")))

(define (push-result)
  (list (instruction-command "push rax")))

(define (pop-result)
  (list (instruction-command "pop rax")))

(define (pop-operands)
  (list (instruction-command "pop rdi")
        (instruction-command "pop rax")))

(define (return)
  (list (instruction-command "ret")))

(define (generate-add)
  (list (instruction-command "add rax, rdi")))

(define (generate-sub)
  (list (instruction-command "sub rax, rdi")))

(define (generate-mul)
  (list (instruction-command "imul rax, rdi")))

(define (generate-div)
  (list (instruction-command "cqo")
        (instruction-command "div rdi")))

(define (generate-eq)
  (list (instruction-command "cmp rax, rdi")
        (instruction-command "sete al")
        (instruction-command "movzb rax, al")))

(define (generate-neq)
  (list (instruction-command "cmp rax, rdi")
        (instruction-command "setne al")
        (instruction-command "movzb rax, al")))

(define (generate-lt)
  (list (instruction-command "cmp rax, rdi")
        (instruction-command "setl al")
        (instruction-command "movzb rax, al")))

(define (generate-le)
  (list (instruction-command "cmp rax, rdi")
        (instruction-command "setle al")
        (instruction-command "movzb rax, al")))

(define (generate-equal-0)
  (list (instruction-command  "pop rax")
        (instruction-command  "cmp rax, 0")))

(define (generate-jump-if-equal label)
  (list (instruction-command (format "je ~a" label))))

(define (generate-jump label)
  (list (instruction-command (format "jmp ~a" label))))

(define (generate-load-from-local-variable)
  (list (instruction-command "pop rax")
        (instruction-command "mov rax, [rax]")
        (instruction-command "push rax")))

(define (generate-store-to-local-variable)
  (list (instruction-command "pop rdi")
        (instruction-command "pop rax")
        (instruction-command "mov [rax],rdi")
        (instruction-command "push rdi")))

(define (generate-label label)
  (list (instruction-label (format "~a:" label))))

(define (generate-func-call func)
  (list (instruction-command (format "call ~a" func))
        (instruction-command "push rax")))

(define (generate-func-call-with-args func arg-num)
  (define registers '("r9" "r8" "rcx" "rdx" "rsi" "rdi"))
  (append (append-map (lambda (reg)
                        (list (instruction-command "pop rax")
                              (instruction-command (format "mov ~a, rax" reg))))
                      (take-right registers arg-num))
          (generate-func-call func)))

(define (generate-left-value node)
  (unless (node-local-variable? node)
    (generate-error (format "left value must be local variable: ~a" (object-name node))))

  (list (instruction-command "mov rax, rbp")
        (instruction-command (format "sub rax, ~a" (node-local-variable-offset node)))
        (instruction-command "push rax")))

(define (generate nodes variables)
  (define gen-label
    ((lambda ()
       (define label-num 0)
       (lambda ()
         (set! label-num (add1 label-num))
         (format ".Llabel~a" label-num)))))

  (define (generate-rec node)
    (if (null? node)
        '()
        (cond [(node-number? node) (push (node-number-val node))]
              [(node-local-variable? node)
               (append (generate-left-value node)
                       (generate-load-from-local-variable))]
              [(node-assign? node)
               (append (generate-left-value (node-assign-left node))
                       (generate-rec (node-assign-right node))
                       (generate-store-to-local-variable))]
              [(node-return? node)
               (append (generate-rec (node-return-expr node))
                       (pop-result)
                       (free-local-variables)
                       (return))]
              [(node-if? node)
               (define label-else (gen-label))
               (define label-end (gen-label))
               (append (generate-rec (node-if-conditional node))
                       (generate-equal-0)
                       (generate-jump-if-equal label-else)
                       (generate-rec (node-if-true-clause node))
                       (generate-jump label-end)
                       (generate-label label-else)
                       (generate-rec (node-if-false-clause node))
                       (generate-label label-end))]
              [(node-while? node)
               (define label-start (gen-label))
               (define label-end (gen-label))
               (append (generate-label label-start)
                       (generate-rec (node-while-conditional node))
                       (generate-equal-0)
                       (generate-jump-if-equal label-end)
                       (generate-rec (node-while-body node))
                       (generate-jump label-start)
                       (generate-label label-end))]
              [(node-for? node)
               (define label-start (gen-label))
               (define label-end (gen-label))
               (append (generate-rec (node-for-init node))
                       (pop-result)
                       (generate-label label-start)
                       (generate-rec (node-for-conditional node))
                       (generate-equal-0)
                       (generate-jump-if-equal label-end)
                       (generate-rec (node-for-body node))
                       (generate-rec (node-for-next node))
                       (generate-jump label-start)
                       (generate-label label-end))]
              [(node-block? node)
               (append-map generate-rec (node-block-bodys node))]
              [(node-func-call? node)
               (define func (node-func-call-func node))
               (define args (node-func-call-args node))
               (cond [(null? args) (generate-func-call func)]
                     [else
                      (append (append-map generate-rec args)
                              (generate-func-call-with-args func (length args)))])]
              [(node-operator? node)
               (append (generate-rec (node-operator-left node))
                       (generate-rec (node-operator-right node))
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
                              (generate-error (format "unexpected operator: ~a" (node-operator-op node)))])
                       (push-result))]
              [(node-func-declaration? node)
               (define name (node-func-declaration-name node))
               (define args (node-func-declaration-args node))
               (define body (node-func-declaration-body node))
               (define variables (node-func-declaration-variables node))
               (append (generate-label name)
                       (reserve-local-variables variables)
                       (append-map generate-rec body)
                       (pop-result)
                       (free-local-variables)
                       (return))])))

  (define (format-body body)
    (map (lambda (instr)
           (cond
             [(instruction-label? instr)
              (instruction-label-label instr)]
             [(instruction-command? instr)
              (format "\t~a" (instruction-command-command instr)) ]
             [else
              (generate-error (format "undefined type: ~a@~a" instr body))]))
         body))

  (define body
    (append
     (append-map generate-rec nodes)))

  (string-join
   `(".intel_syntax noprefix"
     ".global main"
     ,@(format-body body))
   "\n"))

(define (compile expr)
  (define-values (node variables) (parse expr))
  (generate node variables))

(module+ main
  (define expr
    (command-line
     #:args (expr)
     expr))

  (displayln (compile expr)))
