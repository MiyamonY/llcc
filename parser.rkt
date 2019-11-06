#lang racket

(require "tokenizer.rkt")
(require "node.rkt")
(require "type.rkt")
(require "variables.rkt")

(provide parse)

(define (token-must-be token-pred tokens input)
  (cond [(empty? tokens)
         (parse-error input (string-length input) "assign ends unexpectedly")]
        [(not (token-pred (first tokens)))
         (parse-error input (token-char-at (first tokens))
                      (format "wrong token: ~a" (object-name token-pred)))]))

(define (parse-error expr char-at msg)
  (raise-user-error
   'parse-error  "\n~a\n~a^\n~a\n" expr (make-string char-at #\space) msg))

(define variables (make-variables))
(define (reset-env)
  (set! variables (make-variables)))

(define (parse input)

  ;; term = num | ident ("(" (expr ("," expr)*) ? ")")? | "(" expr ")""
  (define (term tokens)
    (when (null? tokens)
      (parse-error input (string-length input) "expression ends unexpectedly"))

    (cond [(token-number? (first tokens))
           (values (new-node-number (token-number-num (first tokens))) (rest tokens))]
          [(token-identifier? (first tokens))
           (define name (token-identifier-name (first tokens)))
           (cond [(token-lparen? (cadr tokens))
                  (define remaining (cddr tokens))
                  (cond [(token-rparen? (car remaining))
                         (values (new-node-func-call name '()) (cdr remaining))]
                        [else
                         (define (args-aux exprs tokens)
                           (if (not (token-comma? (first tokens)))
                               (values (reverse exprs) tokens)
                               (let-values ([(expr0 remaining) (expr (cdr tokens))])
                                 (args-aux (cons expr0 exprs)  remaining))))
                         (define-values (expr0 remaining1) (expr remaining))
                         (define-values (args remaining2) (args-aux (list expr0) remaining1))
                         (token-must-be token-rparen? remaining2 input)
                         (values (new-node-func-call name args) (cdr remaining2))])]
                 [else
                  (find-variable variables name)
                  (values (new-node-local-variable name) (cdr tokens))])]
          [(token-lparen? (first tokens))
           (define-values (expr0 remaining) (expr (rest tokens)))
           (token-must-be token-rparen? remaining input)
           (values expr0 (cdr remaining))]
          [else
           (parse-error input (token-char-at (first tokens)) "token must be number or ( or identifier")]))

  ;; unary = ("+" | "-")? term
  ;;        | ("&" | "*") unary
  ;;        | "sizeof" unary
  (define (unary tokens)
    (when (null? tokens)
      (parse-error input (string-length input) "expression ends unexpectedly"))

    (define unary-operator (first tokens))
    (cond [(token-plus? unary-operator) (term (rest tokens))]
          [(token-minus? unary-operator)
           (define-values (term0 remaining) (term (rest tokens)))
           (values (node-sub (new-node-number 0) term0) remaining)]
          [(token-addr? unary-operator)
           (define-values (unary0 remaining) (unary (rest tokens)))
           (values (node-addr unary0) remaining)]
          [(token-deref? unary-operator)
           (define-values (unary0 remaining) (unary (rest tokens)))
           (values (node-deref unary0) remaining)]
          [(token-sizeof? unary-operator)
           (define-values (unary0 remaining) (unary (rest tokens)))
           (values (node-sizeof unary0) remaining)]
          [else (term tokens)]))

  ;; mul = unary ("*" unary | "/" unary)*
  (define (mul tokens)
    (define (mul-rec unary0 tokens)
      (cond [(null? tokens) (values unary0 tokens)]
            [(or (token-mul? (car tokens)) (token-div? (car tokens)))
             (define-values (unary1 remaining) (unary (cdr tokens)))
             (mul-rec (new-node-operator (token-operator-op (car tokens)) unary0 unary1) remaining)]
            [else (values unary0 tokens)]))
    (call-with-values (lambda () (unary tokens)) mul-rec))

  ;; add = mul ("+" mul | "-" mul)*
  (define (add tokens)
    (define (add-rec mul0 tokens)
      (cond [(null? tokens) (values mul0 tokens)]
            [(or (token-plus? (first tokens)) (token-minus? (first tokens)))
             (define operator (first tokens))
             (define-values (mul1 remaining) (mul (rest tokens)))
             (add-rec (new-node-operator (token-operator-op operator) mul0 mul1) remaining)]
            [else (values mul0 tokens)]))

    (call-with-values (lambda () (mul tokens)) add-rec))

  ;; relational = add ("<" add | "<=" add | ">" add | ">=" add)*
  (define (relational tokens)
    (define (relational-rec add0 tokens)
      (cond [(empty? tokens) (values add0 tokens)]
            [(or (token-lt? (first tokens)) (token-le? (first tokens)))
             (define operator (first tokens))
             (define-values (add1 remaining) (add (rest tokens)))
             (relational-rec (new-node-operator (token-operator-op operator) add0 add1) remaining)]
            [(or (token-gt? (first tokens)) (token-ge? (first tokens)))
             (define operator (first tokens))
             (define-values (add1 remaining) (add (rest tokens)))
             (relational-rec (new-node-operator
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
             (values (new-node-assign equ0 assign0) remaining)]
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
           (pointers (pointer-of base) (rest tokens))]
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
           (define-values (ty remaining) (pointers int (rest tokens)))

           (token-must-be token-identifier? remaining input)
           (define name (token-identifier-name (first remaining)))
           (assign-variable variables name ty)
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
      (define-values (ty remaining) (pointers int (drop tokens 2)))
      (token-must-be token-identifier?  remaining input)
      (define name (token-identifier-name (first remaining)))
      (assign-variable variables name ty)
      (define arg (new-node-local-variable name))
      (values arg (rest remaining)))
    (define cont* (star cont token-comma?))

    (cond [(token-int? (first tokens))
           (define-values (ty remaining) (pointers int (rest tokens)))
           (token-must-be token-identifier? remaining input)
           (define name (token-identifier-name (first remaining)))
           (assign-variable variables name ty)
           (define arg (new-node-local-variable name))
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
           (values (node-func-declaration name args (node-block stmts) variables) (rest remaining1))]
          [else (values '() tokens)]))

  ;; program = declaration*
  (define (program tokens)
    (define declaration* (star declaration token-int?))
    (declaration* tokens))

  (define-values (nodes remaining) (program (tokenize input)))
  (when (not (null? remaining))
    (parse-error input (token-char-at (first remaining)) "There exists unconsumed tokens"))
  (values variables nodes))

(module+ test
  (require rackunit)

  (define (node-add left right)
    (new-node-operator "+" left right))

  (define (node-mul left right)
    (new-node-operator "*" left right))

  (define (node-div left right)
    (new-node-operator "/" left right))

  (define (node-main body variables)
    (node-func-declaration "main" '() (node-block body) variables))

  (define (variable-int name offset)
    (variable name int offset))

  (define (parse-node input)
    (define-values (_ node) (parse input))
    node)

  (test-equal? "num"
               (parse-node "int main( ) {return 12;}")
               (list
                (node-main (list (node-return (new-node-number 12)))
                           (make-hash))))

  (test-equal? "identifier" (parse-node "int main() {int x; x;}")
               (list
                (node-main (list
                            (node-variable-declaration "x")
                            (new-node-local-variable "x"))
                           (make-hash `(("x" . ,(variable-int "x" 8)))))))

  (test-equal? "func call without args" (parse-node "int main(){test();}")
               (list
                (node-main (list (new-node-func-call "test" '()))
                           (make-hash))))

  (test-equal? "func call with one arg" (parse-node "int main(){test(1);}")
               (list
                (node-main (list (new-node-func-call "test" (list (new-node-number 1))))
                           (make-hash))))

  (test-equal? "func call with max args" (parse-node "int main(){test(1,2,3,4,5,6);}")
               (list
                (node-main
                 (list (new-node-func-call "test" (list (new-node-number 1)
                                                        (new-node-number 2)
                                                        (new-node-number 3)
                                                        (new-node-number 4)
                                                        (new-node-number 5)
                                                        (new-node-number 6))))
                 (make-hash))))

  (test-equal? "parens" (parse-node "int main(){(1+2);}")
               (list (node-main (list (node-add (new-node-number 1) (new-node-number 2)))
                                (make-hash))))

  (test-equal? "unary plus" (parse-node "int main(){+1;}")
               (list (node-main (list (new-node-number 1))
                                (make-hash))))

  (test-equal? "unary minus" (parse-node "int main(){-3;}")
               (list (node-main
                      (list (node-sub (new-node-number 0) (new-node-number 3)))
                      (make-hash))))

  (test-equal? "unary sizeof" (parse-node "int main(){sizeof(-3);}")
               (list (node-main
                      (list (node-sizeof (node-sub (new-node-number 0) (new-node-number 3))))
                      (make-hash))))

  (test-equal? "unary addr" (parse-node "int main(){ int x; &x + &3;}")
               (list (node-main
                      (list
                       (node-variable-declaration "x")
                       (node-add (node-addr (new-node-local-variable "x"))
                                 (node-addr (new-node-number 3))))
                      (make-hash `(("x" . ,(variable-int "x" 8)))))))

  (test-equal? "unary deref" (parse-node "int main(){ int x; int y; x=3; *(&x-8); *&y; &*&x + &(3+4);}")
               (list (node-main
                      (list
                       (node-variable-declaration "x")
                       (node-variable-declaration "y")
                       (new-node-assign (new-node-local-variable "x") (new-node-number 3))
                       (node-deref (node-sub (node-addr (new-node-local-variable "x")) (new-node-number 8)))
                       (node-deref (node-addr (new-node-local-variable "y")))
                       (node-add (node-addr (node-deref (node-addr (new-node-local-variable "x"))))
                                 (node-addr (node-add (new-node-number 3)
                                                      (new-node-number 4)))))
                      (make-hash `(("x" . ,(variable-int "x" 8))
                                   ("y" . ,(variable-int "y" 16)))))))


  (test-equal? "mul muls and divs" (parse-node "int main(){2*3/2*3/2;}")
               (list (node-main
                      (list
                       (node-div
                        (node-mul
                         (node-div
                          (node-mul
                           (new-node-number 2) (new-node-number 3))
                          (new-node-number 2)) (new-node-number 3)) (new-node-number 2)))
                      (make-hash))))

  (test-equal? "relationals" (parse-node "int main(){1<2<=3>=2>1;}")
               (list (node-main
                      (list
                       (node-lt (new-node-number 1)
                                (node-le (new-node-number 2)
                                         (node-le
                                          (node-lt (new-node-number 1) (new-node-number 2)) (new-node-number 3)))))
                      (make-hash))))

  (test-equal? "euqalities" (parse-node "int main(){1==2 != 3;}")
               (list (node-main (list (node-neq (node-eq (new-node-number 1) (new-node-number 2)) (new-node-number 3)))
                                (make-hash))))

  (test-equal? "assign" (parse-node "int main(){int x; int y; x=y=1;}")
               (list (node-main
                      (list
                       (node-variable-declaration "x")
                       (node-variable-declaration "y")
                       (new-node-assign (new-node-local-variable "x")
                                        (new-node-assign (new-node-local-variable "y") (new-node-number 1))))
                      (make-hash `(("x" . ,(variable-int "x" 8)) ("y" . ,(variable-int "y" 16)))))))

  (test-equal? "return"
               (parse-node "int main(){return 3;}")
               (list (node-main
                      (list (node-return (new-node-number 3)))
                      (make-hash))))

  (test-equal? "ifs"
               (parse-node "int main(){if (1 <= 2 ) return 3;if (1 < 2) return 3; else return 4;if(1) 1;}")
               (list (node-main
                      (list
                       (node-if (node-le (new-node-number 1) (new-node-number 2))
                                (node-return (new-node-number 3)) null)
                       (node-if (node-lt (new-node-number 1) (new-node-number 2))
                                (node-return (new-node-number 3)) (node-return (new-node-number 4)))
                       (node-if (new-node-number 1) (new-node-number 1) null))
                      (make-hash))))

  (test-equal? "whiles"
               (parse-node "int main(){int x; while (x<3) return 4;}")
               (list (node-main
                      (list
                       (node-variable-declaration "x")
                       (node-while (node-lt (new-node-local-variable "x") (new-node-number 3))
                                   (node-return (new-node-number 4))))
                      (make-hash `(("x" . ,(variable-int "x" 8)))))))

  (test-equal? "for all"
               (parse-node "int main(){int x; for (x=0; x < 10; x = x + 1) 2 + 3;}")
               (list (node-main
                      (list
                       (node-variable-declaration "x")
                       (node-for (new-node-assign (new-node-local-variable "x") (new-node-number 0))
                                 (node-lt (new-node-local-variable "x") (new-node-number 10))
                                 (new-node-assign (new-node-local-variable "x")
                                                  (node-add
                                                   (new-node-local-variable "x")
                                                   (new-node-number 1)))
                                 (node-add (new-node-number 2) (new-node-number 3))))
                      (make-hash `(("x" . ,(variable-int "x" 8)))))))

  (test-equal? "for while"
               (parse-node "int main(){for (;;) 1;}")
               (list (node-main
                      (list
                       (node-for null
                                 null
                                 null
                                 (new-node-number 1)))
                      (make-hash))))

  (test-equal? "blocks"
               (parse-node "int main(){int **a; {int b; int c; a=1; b=2; c=a+b;return c;}}")
               (list (node-main
                      (list
                       (node-variable-declaration "a")
                       (node-block (list
                                    (node-variable-declaration "b")
                                    (node-variable-declaration "c")
                                    (new-node-assign (new-node-local-variable "a") (new-node-number 1))
                                    (new-node-assign (new-node-local-variable "b") (new-node-number 2))
                                    (new-node-assign (new-node-local-variable "c")
                                                     (node-add (new-node-local-variable "a")
                                                               (new-node-local-variable "b")))
                                    (node-return (new-node-local-variable "c")))))
                      (make-hash
                       `(("a" . ,(variable "a" (pointer-of (pointer-of int)) 8))
                         ("b" . ,(variable-int "b" 16))
                         ("c" . ,(variable-int "c" 24)))))))

  (test-equal? "function declarations"
               (parse-node "int a(int x, int **y){int z; z = x + y; return z+3;} int b(int *y){return 3*y;}
int main(){return a()+b();}")
               (list
                (node-func-declaration
                 "a"
                 (list (new-node-local-variable "x") (new-node-local-variable "y"))
                 (node-block
                  (list
                   (node-variable-declaration "z")
                   (new-node-assign
                    (new-node-local-variable "z")
                    (new-node-operator
                     "+"
                     (new-node-local-variable "x")
                     (new-node-local-variable "y")))
                   (node-return
                    (new-node-operator "+" (new-node-local-variable "z") (new-node-number 3)))))
                 (make-hash `(("x" . ,(variable-int "x" 8))
                              ("y" . ,(variable "y" (pointer-of (pointer-of int)) 16))
                              ("z" . ,(variable-int "z" 24))) ))
                (node-func-declaration
                 "b"
                 (list (new-node-local-variable "y"))
                 (node-block
                  (list
                   (node-return
                    (new-node-operator "*" (new-node-number 3) (new-node-local-variable "y")))))
                 (make-hash `(("y" . ,(variable "y" (pointer-of int) 8)))))
                (node-func-declaration
                 "main"
                 '()
                 (node-block
                  (list
                   (node-return
                    (new-node-operator "+" (new-node-func-call "a" '()) (new-node-func-call "b" '())))))
                 (make-hash))))

  (define input-for-parser-error
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
      "int main(){int ; int x; int y; x + y;}"
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
            input-for-parser-error)

  (define input-for-varaibles-error
    '("int main(){int x; x + y;}"
      "int main(){int x; int x; int y; 3;}"
      "int main(int x){int x; x = 3; return 2*x;}"
      "int main(int x, int x){return 2*x;}"))
  (for-each (lambda (input)
              (check-exn #rx"variables-error"
                         (lambda () (parse input))
                         (format "No exception raised: ~a" input)))
            input-for-varaibles-error))
