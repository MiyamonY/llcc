#lang racket

(provide
 tokenize
 token-char-at
 token-operator-op
 token-plus?
 token-minus?
 token-mul?
 token-div?
 token-addr?
 token-deref?
 token-comma?
 token-semicolon?
 token-eq?
 token-neq?
 token-lt?
 token-le?
 token-gt?
 token-ge?
 token-lparen?
 token-rparen?
 token-lcurly-brace?
 token-rcurly-brace?
 token-lbracket?
 token-rbracket?
 token-assign?
 token-return?
 token-if?
 token-else?
 token-for?
 token-while?
 token-int?
 token-identifier?
 token-identifier-name
 token-number?
 token-number-num
 token-sizeof?)

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

(struct token-type token (type)
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

(struct token-bracket token (val)
  #:transparent)

(define (token-lbracket char-at)
  (token-bracket char-at "["))

(define (token-rbracket char-at)
  (token-bracket char-at "]"))

(define (token-lbracket? token)
  (and (token-bracket? token) (equal? (token-bracket-val token) "[")))

(define (token-rbracket? token)
  (and (token-bracket? token) (equal? (token-bracket-val token) "]")))

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

(define (token-addr char-at)
  (token-operator char-at "&"))

(define (token-sizeof char-at)
  (token-operator char-at "sizeof"))

(define (token-plus? token)
  (and (token-operator? token) (equal? (token-operator-op token) "+")))

(define (token-add? token)
  (token-plus? token))

(define (token-minus? token)
  (and (token-operator? token) (equal? (token-operator-op token) "-")))

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

(define (token-addr? token)
  (and (token-operator? token) (equal? (token-operator-op token) "&")))

(define (token-deref? token)
  (token-mul? token))

(define (token-sizeof? token)
  (and (token-operator? token) (equal? (token-operator-op token) "sizeof")))

(define (token-int char-at)
  (token-type char-at "int"))

(define (token-int? token)
  (and (token-type? token) (equal? (token-type-type token) "int")))

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
          [(member (peek lst) '(#\space #\tab #\vtab #\page #\return #\newline))
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
          [(equal? (peek lst) #\[)
           (cons (token-lbracket char-at) (tokenize-rec (rest lst) (add1 char-at)))]
          [(equal? (peek lst) #\])
           (cons (token-rbracket char-at) (tokenize-rec (rest lst) (add1 char-at)))]
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
                  (cons (token-gt char-at) (tokenize-rec (rest lst) (add1 char-at)))])]
          [(equal? (peek lst) #\;)
           (cons (token-semicolon char-at) (tokenize-rec (rest lst) (add1 char-at)))]
          [(equal? (peek lst) #\{)
           (cons (token-lcurly-brace char-at) (tokenize-rec (rest lst) (add1 char-at)))]
          [(equal? (peek lst) #\})
           (cons (token-rcurly-brace char-at) (tokenize-rec (rest lst) (add1 char-at)))]
          [(equal? (peek lst) #\&)
           (cons (token-addr char-at) (tokenize-rec (rest lst) (add1 char-at)))]
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
               ["int" (token-int char-at)]
               ["sizeof" (token-sizeof char-at)]
               [_ (token-identifier char-at keyword)]))
           (cons token (tokenize-rec remaining (+ (length taken) char-at)))]
          (else
           (tokenize-error expr char-at "unexpected value"))))

  (tokenize-rec (string->list expr) 0))

(module+ test
  (test-true "test token-lbracket?" (token-lbracket? (token-lbracket 10)))

  (test-exn "invalid operator "#rx"tokenize-error" (lambda () (tokenize "1 !! 2")))
  (check-exn #rx"tokenize-error" (lambda () (tokenize "~"))))
