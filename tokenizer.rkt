#lang racket

(provide
 tokenize
 token-char-at
 token-operator-op
 token-add?
 token-sub?
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

(require (for-syntax racket/syntax))

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

(define-syntax (define-token-operator stx)
  (syntax-case stx ()
    [(_ name op)
     (with-syntax ([constructor (format-id #'name "token-~a" #'name)]
                   [operator-token? (format-id #'name "token-~a?" #'name)])
       #'(begin
           (define (constructor char-at)
             (token-operator char-at op))
           (define (operator-token? token)
             (and (token-operator? token) (equal? (token-operator-op token) op)))))]))

(define-token-operator add "+")
(define-token-operator sub "-")
(define-token-operator mul "*")
(define-token-operator div "/")
(define-token-operator eq "==")
(define-token-operator neq "!=")
(define-token-operator lt "<")
(define-token-operator le "<=")
(define-token-operator gt ">")
(define-token-operator ge ">=")
(define-token-operator assign "=")
(define-token-operator sizeof "sizeof")
(define-token-operator addr "&")

(define (token-deref? token)
  (token-mul? token))

(struct token-type token (type)
  #:transparent)

(define-syntax (define-token-type stx)
  (syntax-case stx ()
    [(_ name type)
     (with-syntax ([constructor (format-id #'name "token-~a" #'name)]
                   [operator-token? (format-id #'name "token-~a?" #'name)])
       #'(begin
           (define (constructor char-at)
             (token-type char-at type))
           (define (operator-token? token)
             (and (token-type? token) (equal? (token-type-type token) type)))))]))

(define-token-type int "int")

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
