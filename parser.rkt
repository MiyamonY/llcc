#lang racket

(require "tokenizer.rkt")

(provide
 parse
 node-add?
 node-sub?
 node-mul?
 node-div?
 node-eq?
 node-neq?
 node-lt?
 node-le?
 node-addr?
 node-deref?
 (struct-out node-local-variable)
 (struct-out node-number)
 (struct-out node-assign)
 (struct-out node-return)
 (struct-out node-if)
 (struct-out node-while)
 (struct-out node-for)
 (struct-out node-block)
 (struct-out node-func-call)
 (struct-out node-operator)
 (struct-out node-unary-operator)
 (struct-out node-func-declaration)
 (struct-out node-variable-declaration))

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

(struct node-variable-declaration node (var)
  #:transparent)

(struct node-operator node (op left right)
  #:transparent)

(struct node-unary-operator node (op unary)
  #:transparent)

(struct type node (type base)
  #:transparent)

(struct variable (name type offset)
  #:transparent)

(define (node-sub left right)
  (node-operator "-" left right))

(define (node-eq left right)
  (node-operator "==" left right))

(define (node-neq left right)
  (node-operator "!=" left right))

(define (node-lt left right)
  (node-operator "<" left right))

(define (node-le left right)
  (node-operator "<=" left right))

(define (node-addr node)
  (node-unary-operator "&" node))

(define (node-deref node)
  (node-unary-operator "*" node))

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

(define (node-addr? node)
  (and (node-unary-operator? node) (equal? (node-unary-operator-op node) "&")))

(define (node-deref? node)
  (and (node-unary-operator? node) (equal? (node-unary-operator-op node) "*")))

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

(define (parse-error expr char-at msg)
  (raise-user-error
   'parse-error  "\n~a\n~a^\n~a\n" expr (make-string char-at #\space) msg))

(define (parse input)
  (define variables (make-hash))
  (define offset 0)

  (define (new-offset)
    (set! offset (+ offset 8))
    offset)

  (define (reset-env)
    (set! variables (make-hash))
    (set! offset 0))

  (define (reference-variable name)
    (if (hash-has-key? variables name)
        (variable-offset (hash-ref variables name))
        #f))

  (define (assign-variable name type)
    (if (hash-has-key? variables name)
        #f
        (let ([offset (new-offset)])
          (hash-set! variables name (variable name type offset))
          (node-local-variable name offset))))

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
                  (define offset (reference-variable name))
                  (when (not offset)
                    (parse-error input (token-char-at (first tokens))
                                 (format "variable: ~a is not declared" name)))
                  (values (node-local-variable name offset) (cdr tokens))])]
          [(token-lparen? (first tokens))
           (define-values (expr0 remaining) (expr (rest tokens)))
           (token-must-be token-rparen? remaining input)
           (values expr0 (cdr remaining))]
          [else
           (parse-error input (token-char-at (first tokens)) "token must be number or ( or identifier")]))

  ;; unary = ("+" | "-")? term | ("&" | "*") unary
  (define (unary tokens)
    (when (null? tokens)
      (parse-error input (string-length input) "expression ends unexpectedly"))

    (define unary-operator (first tokens))
    (cond [(token-plus? unary-operator) (term (rest tokens))]
          [(token-minus? unary-operator)
           (define-values (term0 remaining) (term (rest tokens)))
           (values (node-sub (node-number 0) term0) remaining)]
          [(token-addr? unary-operator)
           (define-values (unary0 remaining) (unary (rest tokens)))
           (values (node-addr unary0) remaining)]
          [(token-deref? unary-operator)
           (define-values (unary0 remaining) (unary (rest tokens)))
           (values (node-deref unary0) remaining)]
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
            [(or (token-plus? (first tokens)) (token-minus? (first tokens)))
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

  ;; expr = assign
  (define (expr tokens)
    (assign tokens))

  (define (star a continue?)
    (lambda (tokens)
      (define (rec nodes tokens)
        (cond [(null? tokens) (values (reverse nodes) tokens)]
              [(continue? (first tokens))
               (define-values (node remaining) (a tokens))
               (rec (cons node nodes) remaining)]
              [else
               (values (reverse nodes) tokens)]))
      (rec '() tokens)))

  ;; pointers =  ("*")*
  (define (pointers base tokens)
    (cond [(token-mul? (first tokens))
           (pointers (type 'pointer base) (rest tokens))]
          [else
           (values base tokens)]))

  ;; stmt = expr ";"
  ;;      | "int" pointers ident ";"
  ;;      | "return" expr ";"
  ;;      | "if" "(" expr ")" stmt ("else" stmt)?
  ;;      | "while" "(" expr ")" stmt
  ;;      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
  ;;      | "{" stmt* "}"
  (define (stmt tokens)
    (cond [(empty? tokens)
           (parse-error input (string-length input) "stmt is empty")]
          [(token-int? (first tokens))
           (define-values (ty remaining) (pointers (type 'int '()) (rest tokens)))

           (token-must-be token-identifier? remaining input)
           (define name (token-identifier-name (first remaining)))
           (define var (assign-variable name ty))
           (when (not var)
             (parse-error input (token-char-at (first remaining))
                          (format "variable: ~a is already assigned" name)))

           (token-must-be token-semicolon? (rest remaining) input)
           (values (node-variable-declaration name) (drop remaining 2))]
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
           (define stmt* (star stmt (compose not token-rcurly-brace?)))
           (define-values (stmts remaining) (stmt* (rest tokens)))
           (token-must-be token-rcurly-brace? remaining input)
           (values (node-block stmts) (rest remaining))]
          [else
           (define-values (expr0 remaining) (expr tokens))
           (token-must-be token-semicolon? remaining input)
           (values expr0 (rest remaining))]))

  ;; arguments = "int" pointer ident ("," "int" pointer indent)*
  ;;           | e
  (define (arguments tokens)
    (define (cont tokens)
      (token-must-be token-comma? tokens input)
      (token-must-be token-int? (rest tokens) input)
      (define-values (ty remaining) (pointers (type 'int '()) (drop tokens 2)))
      (token-must-be token-identifier?  remaining input)
      (define name (token-identifier-name (first remaining)))
      (values (assign-variable name ty) (rest remaining)))
    (define cont* (star cont token-comma?))

    (cond [(token-int? (first tokens))
           (define-values (ty remaining) (pointers (type 'int '()) (rest tokens)))
           (token-must-be token-identifier? remaining input)
           (define name (token-identifier-name (first remaining)))
           (define arg (assign-variable name ty))
           (define-values (args remaining1) (cont* (rest remaining)))
           (values (cons arg args) remaining1)]
          [else
           (values '() tokens)]))

  ;; declaration = "int" ident (" arguments ")" "{" (stmt)* "}"
  (define (declaration tokens)
    (cond [(null? tokens) (values '() '())]
          [(token-int? (first tokens))
           (define stmt* (star stmt (compose not token-rcurly-brace?)))

           (reset-env)
           (token-must-be token-identifier? (rest tokens) input)
           (define name (token-identifier-name (first (rest tokens))))
           (token-must-be token-lparen? (drop tokens 2) input)
           (define-values (args remaining0) (arguments (drop tokens 3)))
           (token-must-be token-rparen? remaining0 input)
           (token-must-be token-lcurly-brace? (drop remaining0 1) input)
           (define-values (stmts remaining1) (stmt* (drop remaining0 2)))
           (token-must-be token-rcurly-brace? remaining1 input)
           (values (node-func-declaration name args stmts variables) (rest remaining1))]
          [else (values '() tokens)]))

  ;; program = declaration*
  (define (program tokens)
    (define declaration* (star declaration token-int?))
    (declaration* tokens))

  (define-values (nodes remaining) (program (tokenize input)))
  (when (not (null? remaining))
    (parse-error input (token-char-at (first remaining)) "There exists unconsumed tokens"))
  nodes)

(module+ test
  (require rackunit)

  (define (node-add left right)
    (node-operator "+" left right))

  (define (node-mul left right)
    (node-operator "*" left right))

  (define (node-div left right)
    (node-operator "/" left right))

  (define (node-main body variables)
    (node-func-declaration "main" '() body variables))

  (define (variable-int name offset)
    (variable name (type 'int '()) offset))

  (test-equal? "num"
               (parse "int main( ) {return 12;}")
               (list
                (node-main (list (node-return (node-number 12)))
                           (make-hash))))

  (test-equal? "identifier" (parse "int main() {int x; x;}")
               (list
                (node-main (list
                            (node-variable-declaration "x")
                            (node-local-variable "x" 8))
                           (make-hash `(("x" . ,(variable-int "x" 8)))))))

  (test-equal? "func call without args" (parse "int main(){test();}")
               (list
                (node-main (list (node-func-call "test" '()))
                           (make-hash))))

  (test-equal? "func call with one arg" (parse "int main(){test(1);}")
               (list
                (node-main (list (node-func-call "test" (list (node-number 1))))
                           (make-hash))))

  (test-equal? "func call with max args" (parse "int main(){test(1,2,3,4,5,6);}")
               (list
                (node-main
                 (list (node-func-call "test" (list (node-number 1)
                                                    (node-number 2)
                                                    (node-number 3)
                                                    (node-number 4)
                                                    (node-number 5)
                                                    (node-number 6))))
                 (make-hash))))

  (test-equal? "parens" (parse "int main(){(1+2);}")
               (list (node-main (list (node-add (node-number 1) (node-number 2)))
                                (make-hash))))

  (test-equal? "unary plus" (parse "int main(){+1;}")
               (list (node-main (list (node-number 1))
                                (make-hash))))

  (test-equal? "unary minus" (parse "int main(){-3;}")
               (list (node-main
                      (list (node-sub (node-number 0) (node-number 3)))
                      (make-hash))))

  (test-equal? "unary addr" (parse "int main(){ int x; &x + &3;}")
               (list (node-main
                      (list
                       (node-variable-declaration "x")
                       (node-add (node-addr (node-local-variable "x" 8))
                                 (node-addr (node-number 3))))
                      (make-hash `(("x" . ,(variable-int "x" 8)))))))

  (test-equal? "unary deref" (parse "int main(){ int x; &*&x + &(3+4);}")
               (list (node-main
                      (list
                       (node-variable-declaration "x")
                       (node-add (node-addr (node-deref (node-addr (node-local-variable "x" 8))))
                                 (node-addr (node-add (node-number 3)
                                                      (node-number 4)))))
                      (make-hash `(("x" . ,(variable-int "x" 8)))))))


  (test-equal? "mul muls and divs" (parse "int main(){2*3/2*3/2;}")
               (list (node-main
                      (list
                       (node-div
                        (node-mul
                         (node-div
                          (node-mul
                           (node-number 2) (node-number 3))
                          (node-number 2)) (node-number 3)) (node-number 2)))
                      (make-hash))))

  (test-equal? "relationals" (parse "int main(){1<2<=3>=2>1;}")
               (list (node-main
                      (list
                       (node-lt (node-number 1)
                                (node-le (node-number 2)
                                         (node-le
                                          (node-lt (node-number 1) (node-number 2)) (node-number 3)))))
                      (make-hash))))

  (test-equal? "euqalities" (parse "int main(){1==2 != 3;}")
               (list (node-main (list (node-neq (node-eq (node-number 1) (node-number 2)) (node-number 3)))
                                (make-hash))))

  (test-equal? "assign" (parse "int main(){int x; int y; x=y=1;}")
               (list (node-main
                      (list
                       (node-variable-declaration "x")
                       (node-variable-declaration "y")
                       (node-assign (node-local-variable "x" 8)
                                    (node-assign (node-local-variable "y" 16) (node-number 1))))
                      (make-hash `(("x" . ,(variable-int "x" 8)) ("y" . ,(variable-int "y" 16)))))))

  (test-equal? "return"
               (parse "int main(){return 3;}")
               (list (node-main
                      (list (node-return (node-number 3)))
                      (make-hash))))

  (test-equal? "ifs"
               (parse "int main(){if (1 <= 2 ) return 3;if (1 < 2) return 3; else return 4;if(1) 1;}")
               (list (node-main
                      (list
                       (node-if (node-le (node-number 1) (node-number 2))
                                (node-return (node-number 3)) null)
                       (node-if (node-lt (node-number 1) (node-number 2))
                                (node-return (node-number 3)) (node-return (node-number 4)))
                       (node-if (node-number 1) (node-number 1) null))
                      (make-hash))))

  (test-equal? "whiles"
               (parse "int main(){int x; while (x<3) return 4;}")
               (list (node-main
                      (list
                       (node-variable-declaration "x")
                       (node-while (node-lt (node-local-variable "x" 8) (node-number 3))
                                   (node-return (node-number 4))))
                      (make-hash `(("x" . ,(variable-int "x" 8)))))))

  (test-equal? "for all"
               (parse "int main(){int x; for (x=0; x < 10; x = x + 1) 2 + 3;}")
               (list (node-main
                      (list
                       (node-variable-declaration "x")
                       (node-for (node-assign (node-local-variable "x" 8) (node-number 0))
                                 (node-lt (node-local-variable "x" 8) (node-number 10))
                                 (node-assign (node-local-variable "x" 8)
                                              (node-add
                                               (node-local-variable "x" 8)
                                               (node-number 1)))
                                 (node-add (node-number 2) (node-number 3))))
                      (make-hash `(("x" . ,(variable-int "x" 8)))))))

  (test-equal? "for while"
               (parse "int main(){for (;;) 1;}")
               (list (node-main
                      (list
                       (node-for null
                                 null
                                 null
                                 (node-number 1)))
                      (make-hash))))

  (test-equal? "blocks"
               (parse "int main(){int **a; {int b; int c; a=1; b=2; c=a+b;return c;}}")
               (list (node-main
                      (list
                       (node-variable-declaration "a")
                       (node-block (list
                                    (node-variable-declaration "b")
                                    (node-variable-declaration "c")
                                    (node-assign (node-local-variable "a" 8) (node-number 1))
                                    (node-assign (node-local-variable "b" 16) (node-number 2))
                                    (node-assign (node-local-variable "c" 24)
                                                 (node-add (node-local-variable "a" 8)
                                                           (node-local-variable "b" 16)))
                                    (node-return (node-local-variable "c" 24)))))
                      (make-hash
                       `(("a" . ,(variable "a" (type 'pointer (type 'pointer (type 'int '()))) 8))
                         ("b" . ,(variable-int "b" 16))
                         ("c" . ,(variable-int "c" 24)))))))

  (test-equal? "function declarations"
               (parse "int a(int x, int **y){int z; z = x + y; return z+3;} int b(int *y){return 3*y;}
int main(){return a()+b();}")
               (list
                (node-func-declaration
                 "a"
                 (list (node-local-variable "x" 8) (node-local-variable "y" 16))
                 (list
                  (node-variable-declaration "z")
                  (node-assign
                   (node-local-variable "z" 24)
                   (node-operator
                    "+"
                    (node-local-variable "x" 8)
                    (node-local-variable "y" 16)))
                  (node-return
                   (node-operator "+" (node-local-variable "z" 24) (node-number 3))))
                 (make-hash `(("x" . ,(variable-int "x" 8))
                              ("y" . ,(variable-int "y" (type 'pointer (type 'pointer (type 'int '()))) 16))
                              ("z" . ,(variable-int "z" 24))) ))
                (node-func-declaration
                 "b"
                 (list (node-local-variable "y" 8))
                 (list
                  (node-return
                   (node-operator "*" (node-number 3) (node-local-variable "y" 8))))
                 (make-hash `(("y" . ,(variable "y" (type 'pointer (type 'int '())) 8)))))
                (node-func-declaration
                 "main"
                 '()
                 (list
                  (node-return
                   (node-operator "+" (node-func-call "a" '()) (node-func-call "b" '()))))
                 (make-hash))))

  (define input-for-exn
    '("int main(){12+;}"
      "int main(){12+ +;}"
      "int main(){12+ -;}"
      "int main(){;}"
      "int main(){ 12+ 34 12;}"
      "int main(){ 12+ 34}"
      "int main(){ 12+ 34=;}"
      "int main(){ 12+ 34<;}"
      "int main(){if(1+2) 3 else return 4;}"
      "int main(){if(1 return 3; else retunr 4;}"
      "int main(){if(1+2) 3; else }"
      "int main(){if(1+2); }"
      "int main(){if(1+2}"
      "int main(){3/}"
      "int main(){int x; x + y;}"
      "int main(){int ; int x; int y; x + y;}"
      "int main(){int x; int x; int y; 3;}"
      "int main(){3/-}"
      "int main(){{1+2;}"
      "int main(){"
      "int main){}"
      "int main({}"
      "int main(int x int y){4+3;}"
      "int main(int x,){2+3;}"
      "main(int x, int y) {2+3;}"
      "main(int x, y) {2+3;}"
      "(){}"
      "12"))

  (for-each (lambda (input)
              (check-exn #rx"parse-error"
                         (lambda () (parse input))
                         (format "No exception raised: ~a" input)))
            input-for-exn))
