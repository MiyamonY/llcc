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

(define (star a continue?)
  (lambda (tokens variables)
    (let rec ((nodes '())
              (tokens tokens)
              (variables variables))
      (cond [(null? tokens) (values (reverse nodes) tokens variables)]
            [(continue? (first tokens))
             (define-values (node remaining variables0) (a tokens variables))
             (rec (cons node nodes) remaining variables0)]
            [else
             (values (reverse nodes) tokens variables)]))))

(define (star-with-different-variables a continue?)
  (lambda (tokens variables)
    (let rec ((nodes '())
              (tokens tokens))
      (cond [(null? tokens) (values (reverse nodes) tokens variables)]
            [(continue? (first tokens))
             (define-values (node remaining _) (a tokens variables))
             (rec (cons node nodes) remaining)]
            [else
             (values (reverse nodes) tokens variables)]))))

(define input "")

;; term = num | ident ("(" (expr ("," expr)*) ? ")")? | "(" expr ")""
(define (term tokens variables)
  (when (null? tokens)
    (parse-error input (string-length input) "expression ends unexpectedly"))

  (cond [(token-number? (first tokens))
         (define number-node (new-node-number (token-number-num (first tokens))))
         (values number-node (rest tokens) variables)]
        [(token-identifier? (first tokens))
         (define name (token-identifier-name (first tokens)))
         (cond [(token-lparen? (cadr tokens))
                (define remaining (cddr tokens))
                (cond [(token-rparen? (car remaining))
                       (define func-call-node (new-node-func-call name '()))
                       (values func-call-node (cdr remaining) variables)]
                      [else
                       (define (args-aux exprs tokens variables)
                         (if (not (token-comma? (first tokens)))
                             (values (reverse exprs) tokens variables)
                             (let-values ([(expr0 remaining variables0) (expr (cdr tokens) variables)])
                               (args-aux (cons expr0 exprs) remaining variables0))))
                       (define-values (expr0 remaining1 variables0) (expr remaining variables))
                       (define-values (args remaining2 variables1) (args-aux (list expr0) remaining1 variables0))
                       (token-must-be token-rparen? remaining2 input)
                       (define func-call-node (new-node-func-call name args))
                       (values func-call-node (cdr remaining2) variables1)])]
               [else
                (find-variable variables name)
                (define local-variable-node (new-node-local-variable name))
                (values local-variable-node (cdr tokens) variables)])]
        [(token-lparen? (first tokens))
         (define-values (expr0 remaining variables0) (expr (rest tokens) variables))
         (token-must-be token-rparen? remaining input)
         (values expr0 (cdr remaining) variables0)]
        [else
         (parse-error input (token-char-at (first tokens)) "token must be number or ( or identifier")]))

;; unary = ("+" | "-")? term
;;        | ("&" | "*") unary
;;        | "sizeof" unary
(define (unary tokens variables)
  (when (null? tokens)
    (parse-error input (string-length input) "expression ends unexpectedly"))

  (define unary-operator (first tokens))
  (cond [(token-add? unary-operator) (term (rest tokens) variables)]
        [(token-sub? unary-operator)
         (define-values (term0 remaining variables0) (term (rest tokens) variables))
         (define sub-node (node-sub (new-node-number 0) term0))
         (values sub-node remaining variables0)]
        [(token-addr? unary-operator)
         (define-values (unary0 remaining variables0) (unary (rest tokens) variables))
         (values (node-addr unary0) remaining variables0)]
        [(token-deref? unary-operator)
         (define-values (unary0 remaining variables0) (unary (rest tokens) variables))
         (values (node-deref unary0) remaining variables0)]
        [(token-sizeof? unary-operator)
         (define-values (unary0 remaining variables0) (unary (rest tokens) variables))
         (values (node-sizeof unary0) remaining variables0)]
        [else (term tokens variables)]))

;; mul = unary ("*" unary | "/" unary)*
(define (mul tokens variables)
  (define (mul-rec unary0 tokens variables)
    (cond [(null? tokens) (values unary0 tokens variables)]
          [(or (token-mul? (car tokens)) (token-div? (car tokens)))
           (define-values (unary1 remaining variables0) (unary (cdr tokens) variables))
           (define operator-node (node-operator '() (token-operator-op (car tokens)) unary0 unary1))
           (mul-rec operator-node remaining variables0)]
          [else (values unary0 tokens variables)]))

  (call-with-values (lambda () (unary tokens variables)) mul-rec))

;; add = mul ("+" mul | "-" mul)*
(define (add tokens variables)
  (define (add-rec mul0 tokens variables)
    (cond [(null? tokens) (values mul0 tokens variables)]
          [(or (token-add? (first tokens)) (token-sub? (first tokens)))
           (define operator (first tokens))
           (define-values (mul1 remaining variables0) (mul (rest tokens) variables))
           (define operator-node (node-operator '() (token-operator-op operator) mul0 mul1))
           (add-rec operator-node remaining variables0)]
          [else (values mul0 tokens variables)]))

  (call-with-values (lambda () (mul tokens variables)) add-rec))

;; relational = add ("<" add | "<=" add | ">" add | ">=" add)*
(define (relational tokens variables)
  (define (relational-rec add0 tokens variables)
    (cond [(empty? tokens) (values add0 tokens variables)]
          [(or (token-lt? (first tokens)) (token-le? (first tokens)))
           (define operator (first tokens))
           (define-values (add1 remaining variables0) (add (rest tokens) variables))
           (define operator-node
             (node-operator '() (token-operator-op operator) add0 add1))
           (relational-rec operator-node remaining variables0)]
          [(or (token-gt? (first tokens)) (token-ge? (first tokens)))
           (define operator (first tokens))
           (define-values (add1 remaining variables0) (add (rest tokens) variables))
           (define operator-node
             (node-operator '() (reverse-compare (token-operator-op operator)) add1 add0))
           (relational-rec operator-node remaining variables0)]
          [else (values add0 tokens variables)]))

  (call-with-values (lambda () (add tokens variables)) relational-rec))

;; equality = relational ("==" relational | "!=" relational)*
(define (equality tokens variables)
  (define (equality-rec rel0 tokens variables)
    (cond [(empty? tokens) (values rel0 tokens variables)]
          [(token-eq? (first tokens))
           (define-values (rel1 remaining variables0) (relational (rest tokens) variables))
           (equality-rec (node-eq rel0 rel1) remaining variables0)]
          [(token-neq? (first tokens))
           (define-values (rel1 remaining variables0) (relational (rest tokens) variables))
           (equality-rec (node-neq rel0 rel1) remaining variables0)]
          [else (values rel0 tokens variables)]))

  (call-with-values (lambda () (relational tokens variables)) equality-rec))

;; assign = equality ("=" assing)?
(define (assign tokens variables)
  (define (assign-aux equ0 tokens variables)
    (when (empty? tokens)
      (parse-error input (string-length input) "assign ends unexpectedly"))

    (cond [(token-assign? (first tokens))
           (define-values (assign0 remaining variables0)
             (assign (rest tokens) variables))
           (values (new-node-assign equ0 assign0) remaining variables0)]
          [else
           (values equ0 tokens variables)]))

  (call-with-values (lambda () (equality tokens variables)) assign-aux))

;; expr = assign
(define (expr tokens variables)
  (assign tokens variables))

;; pointers =  ("*")*
(define (pointers base tokens)
  (cond [(token-mul? (first tokens))
         (pointers (pointer-of base) (rest tokens))]
        [else
         (values base tokens)]))

;; stmt = expr ";"
;;      | "int" pointers ident ("[" num "]")? ";"
;;      | "return" expr ";"
;;      | "if" "(" expr ")" stmt ("else" stmt)?
;;      | "while" "(" expr ")" stmt
;;      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
;;      | "{" stmt* "}"
(define (stmt tokens variables)
  (cond [(empty? tokens)
         (parse-error input (string-length input) "stmt is empty")]
        [(token-int? (first tokens))
         (define-values (ty remaining) (pointers int (rest tokens)))
         (token-must-be token-identifier? remaining input)
         (define name (token-identifier-name (first remaining)))
         (cond [(token-lbracket? (second remaining))
                (token-must-be token-number? (drop remaining 2)  input)
                (define size (token-number-num (third remaining)))
                (token-must-be token-rbracket? (drop remaining 3) input)
                (token-must-be token-semicolon? (drop remaining 4) input)
                (define variables0 (assign-variable variables name (array-of ty) size))
                (define variable-declaration-node (node-variable-declaration name))
                (values variable-declaration-node (drop remaining 5) variables0)]
               [else
                (token-must-be token-semicolon? (rest remaining) input)
                (define variables0 (assign-variable variables name ty))
                (define variable-declaration-node (node-variable-declaration name))
                (values variable-declaration-node (drop remaining 2) variables0)])]
        [(token-return? (first tokens))
         (define-values (expr0 remaining variables0) (expr (rest tokens) variables))
         (token-must-be token-semicolon? remaining input)
         (values (node-return expr0) (rest remaining) variables0)]
        [(token-if? (first tokens))
         (token-must-be token-lparen? (rest tokens) input)
         (define-values (conditional remaining0 variables0) (expr (cddr tokens) variables))
         (token-must-be token-rparen? remaining0 input)
         (define-values (true-clause remaining1 variables1) (stmt (rest remaining0) variables0))
         (cond [(null? remaining1)
                (define if-node (node-if conditional true-clause null))
                (values if-node remaining1 variables1)]
               [(token-else? (first remaining1))
                (define-values (false-clause remaining2 variables2) (stmt (rest remaining1) variables1))
                (define if-node (node-if conditional true-clause false-clause))
                (values if-node remaining2 variables2)]
               [else
                (define if-node (node-if conditional true-clause null))
                (values if-node remaining1 variables1)])]
        [(token-while? (first tokens))
         (token-must-be token-lparen? (rest tokens) input)
         (define-values (conditional remaining variables0) (expr (rest (rest tokens)) variables))
         (token-must-be token-rparen? remaining input)
         (define-values (body remaining0 variables1) (stmt (rest remaining) variables0))
         (values (node-while conditional body) remaining0 variables1)]
        [(token-for? (first tokens))
         (token-must-be token-lparen? (rest tokens) input)
         (define-values (init remaining0 variables0)
           (if (token-semicolon? (caddr tokens))
               (values null (cddr tokens) variables)
               (expr (cddr tokens) variables)))
         (token-must-be token-semicolon? remaining0 input)

         (define-values (conditional remaining1 variables1)
           (if (token-semicolon? (cadr remaining0))
               (values null (rest remaining0) variables0)
               (expr (rest remaining0) variables0)))
         (token-must-be token-semicolon? remaining1 input)

         (define-values (next remaining2 variables2)
           (if (token-rparen? (cadr remaining1))
               (values null (rest remaining1) variables1)
               (expr (rest remaining1) variables1)))
         (token-must-be token-rparen? remaining2 input)

         (define-values (body remaining3 variables3) (stmt (rest remaining2) variables2))
         (values (node-for init conditional next body) remaining3 variables3)]
        [(token-lcurly-brace? (first tokens))
         (define stmt* (star stmt (compose not token-rcurly-brace?)))
         (define-values (stmts remaining variables0) (stmt* (rest tokens) variables))
         (token-must-be token-rcurly-brace? remaining input)
         (values (node-block stmts) (rest remaining) variables0)]
        [else
         (define-values (expr0 remaining variables0) (expr tokens variables))
         (token-must-be token-semicolon? remaining input)
         (values expr0 (rest remaining) variables0)]))

;; arguments = "int" pointer ident ("," "int" pointer indent)*
;;           | e
(define (arguments tokens variables)
  (define (cont tokens variables)
    (token-must-be token-comma? tokens input)
    (token-must-be token-int? (rest tokens) input)
    (define-values (ty remaining) (pointers int (drop tokens 2)))
    (token-must-be token-identifier?  remaining input)
    (define name (token-identifier-name (first remaining)))
    (define variables0 (assign-variable variables name ty))
    (define arg (new-node-local-variable name))
    (values arg (rest remaining) variables0))
  (define cont* (star cont token-comma?))

  (cond [(token-int? (first tokens))
         (define-values (ty remaining) (pointers int (rest tokens)))
         (token-must-be token-identifier? remaining input)
         (define name (token-identifier-name (first remaining)))
         (define variables0 (assign-variable variables name ty))
         (define arg (new-node-local-variable name))
         (define-values (args remaining1 variables1) (cont* (rest remaining) variables0))
         (values (cons arg args) remaining1 variables1)]
        [else
         (values '() tokens variables)]))

;; declaration = "int" ident (" arguments ")" "{" (stmt)* "}"
(define (declaration tokens variables)
  (define stmt* (star stmt (compose not token-rcurly-brace?)))

  (cond [(null? tokens) (values '() '())]
        [(token-int? (first tokens))
         (token-must-be token-identifier? (rest tokens) input)
         (define name (token-identifier-name (second tokens)))
         (token-must-be token-lparen? (drop tokens 2) input)
         (define-values (args remaining0 variables0) (arguments (drop tokens 3) variables))
         (token-must-be token-rparen? remaining0 input)
         (token-must-be token-lcurly-brace? (drop remaining0 1) input)
         (define-values (stmts remaining1 variables1) (stmt* (drop remaining0 2) variables0))
         (token-must-be token-rcurly-brace? remaining1 input)
         (define func-declaration-node (node-func-declaration name args (node-block stmts) variables1))
         (values func-declaration-node (rest remaining1) variables1)]
        [else (values '() tokens variables)]))

;; program = declaration*
(define (program tokens variables)
  (define declaration*
    (star-with-different-variables declaration token-int?))
  (declaration* tokens variables))

(define (parse prog)
  (set! input prog)
  (define-values (nodes remaining variables) (program (tokenize input) (make-variables)))
  (unless (null? remaining)
    (parse-error input (token-char-at (first remaining)) "There exists unconsumed tokens"))
  (values variables nodes))

(module+ test
  (require rackunit)

  (define (node-main body variables)
    (node-func-declaration "main" '() (node-block body) variables))

  (define (variable-int name offset)
    (variable name int offset 0))

  (define (parse-node input)
    (define-values (_ node) (parse input))
    node)

  (test-case "parse term"
    (test-equal? "number"
                 (parse-node "int main( ) {return 12;}")
                 (list
                  (node-main (list (node-return (new-node-number 12)))
                             (make-variables))))

    (test-equal? "identifier" (parse-node "int main() {int x; x;}")
                 (list
                  (node-main (list
                              (node-variable-declaration "x")
                              (new-node-local-variable "x"))
                             (assign-variable
                              (make-variables)
                              "x" int))))

    (test-equal? "func call without args" (parse-node "int main(){test();}")
                 (list
                  (node-main (list (new-node-func-call "test" '()))
                             (make-variables))))

    (test-equal? "func call with one arg" (parse-node "int main(){test(1);}")
                 (list
                  (node-main (list (new-node-func-call "test" (list (new-node-number 1))))
                             (make-variables))))

    (test-equal? "func call with max args" (parse-node "int main(){test(1,2,3,4,5,6);}")
                 (list
                  (node-main
                   (list (new-node-func-call "test"
                                             (list (new-node-number 1)
                                                   (new-node-number 2)
                                                   (new-node-number 3)
                                                   (new-node-number 4)
                                                   (new-node-number 5)
                                                   (new-node-number 6))))
                   (make-variables)))))

  (test-equal? "parens" (parse-node "int main(){(1+2);}")
               (list (node-main (list (node-add (new-node-number 1) (new-node-number 2)))
                                (make-variables))))

  (test-equal? "unary plus" (parse-node "int main(){+1;}")
               (list (node-main (list (new-node-number 1))
                                (make-variables))))

  (test-equal? "unary minus" (parse-node "int main(){-3;}")
               (list (node-main
                      (list (node-sub (new-node-number 0) (new-node-number 3)))
                      (make-variables))))

  (test-equal? "unary sizeof" (parse-node "int main(){sizeof(-3);}")
               (list (node-main
                      (list (node-sizeof (node-sub (new-node-number 0) (new-node-number 3))))
                      (make-variables))))

  (test-equal? "unary addr" (parse-node "int main(){ int x; &x + &3;}")
               (list (node-main
                      (list
                       (node-variable-declaration "x")
                       (node-add (node-addr (new-node-local-variable "x"))
                                 (node-addr (new-node-number 3))))
                      (assign-variable
                       (make-variables)
                       "x" int))))

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
                      (assign-variable
                       (assign-variable
                        (make-variables)
                        "x" int)
                       "y" int))))

  (test-equal? "mul muls and divs" (parse-node "int main(){2*3/2*3/2;}")
               (list (node-main
                      (list
                       (node-div
                        (node-mul
                         (node-div
                          (node-mul
                           (new-node-number 2) (new-node-number 3))
                          (new-node-number 2)) (new-node-number 3)) (new-node-number 2)))
                      (make-variables))))

  (test-equal? "relationals" (parse-node "int main(){1<2<=3>=2>1;}")
               (list (node-main
                      (list
                       (node-lt (new-node-number 1)
                                (node-le (new-node-number 2)
                                         (node-le
                                          (node-lt (new-node-number 1) (new-node-number 2)) (new-node-number 3)))))
                      (make-variables))))

  (test-equal? "euqalities" (parse-node "int main(){1==2 != 3;}")
               (list (node-main (list (node-neq (node-eq (new-node-number 1) (new-node-number 2)) (new-node-number 3)))
                                (make-variables))))

  (test-equal? "assign" (parse-node "int main(){int x; int y; x=y=1;}")
               (list (node-main
                      (list
                       (node-variable-declaration "x")
                       (node-variable-declaration "y")
                       (new-node-assign (new-node-local-variable "x")
                                        (new-node-assign (new-node-local-variable "y") (new-node-number 1))))
                      (assign-variable
                       (assign-variable
                        (make-variables)
                        "x" int)
                       "y" int))))

  (test-equal? "return"
               (parse-node "int main(){return 3;}")
               (list (node-main
                      (list (node-return (new-node-number 3)))
                      (make-variables))))

  (test-equal? "ifs"
               (parse-node "int main(){if (1 <= 2 ) return 3;if (1 < 2) return 3; else return 4;if(1) 1;}")
               (list (node-main
                      (list
                       (node-if (node-le (new-node-number 1) (new-node-number 2))
                                (node-return (new-node-number 3)) null)
                       (node-if (node-lt (new-node-number 1) (new-node-number 2))
                                (node-return (new-node-number 3)) (node-return (new-node-number 4)))
                       (node-if (new-node-number 1) (new-node-number 1) null))
                      (make-variables))))

  (test-equal? "whiles"
               (parse-node "int main(){int x; while (x<3) return 4;}")
               (list (node-main
                      (list
                       (node-variable-declaration "x")
                       (node-while (node-lt (new-node-local-variable "x") (new-node-number 3))
                                   (node-return (new-node-number 4))))
                      (assign-variable
                       (make-variables)
                       "x" int))))

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
                      (assign-variable
                       (make-variables)
                       "x" int))))

  (test-equal? "for while"
               (parse-node "int main(){for (;;) 1;}")
               (list (node-main
                      (list
                       (node-for null
                                 null
                                 null
                                 (new-node-number 1)))
                      (make-variables))))

  (test-equal? "array declaration"
               (parse-node "int main(){int a[10];}")
               (list (node-main
                      (list
                       (node-variable-declaration "a"))
                      (assign-variable
                       (make-variables)
                       "a" (array-of int) 10))))

  (test-equal? "array declaration"
               (parse-node "int main(){int a[10]; *a = 3;}")
               (list (node-main
                      (list
                       (node-variable-declaration "a")
                       (new-node-assign (node-deref (new-node-local-variable "a")) (new-node-number 3)))
                      (assign-variable
                       (make-variables)
                       "a" (array-of int) 10))))

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
                      (assign-variable
                       (assign-variable
                        (assign-variable
                         (make-variables)
                         "a" (pointer-of (pointer-of int)))
                        "b" int)
                       "c" int))))

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
                    (node-add
                     (new-node-local-variable "x")
                     (new-node-local-variable "y")))
                   (node-return
                    (node-add (new-node-local-variable "z") (new-node-number 3)))))
                 (assign-variable
                  (assign-variable
                   (assign-variable
                    (make-variables)
                    "x" int)
                   "y" (pointer-of (pointer-of int)))
                  "z" int))
                (node-func-declaration
                 "b"
                 (list (new-node-local-variable "y"))
                 (node-block
                  (list
                   (node-return
                    (node-mul (new-node-number 3) (new-node-local-variable "y")))))
                 (assign-variable (make-variables) "y" (pointer-of int) 0))
                (node-func-declaration
                 "main"
                 '()
                 (node-block
                  (list
                   (node-return
                    (node-add (new-node-func-call "a" '()) (new-node-func-call "b" '())))))
                 (make-variables))))

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
      "int main(){int x[;}"
      "int main(){int x];}"
      "int main(){int x[];}"
      "int main(){int [];}"
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
