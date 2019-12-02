#lang racket

(require "variables.rkt")
(require "node.rkt")
(require "type.rkt")

(provide semantics)

(define (semantics-error msg node)
  (raise-user-error
   'type-error  "~a\n:~a" msg node))

(define (analyze variables node)
  (cond [(node-local-variable? node)
         (define name (node-local-variable-name node))
         (define var (find-variable variables name))
         (define base-type (variable-type var))
         (define type (if (> (variable-size var) 0) (array-of base-type) base-type))
         (struct-copy node-local-variable node [type #:parent node-expr type])]
        [(node-number? node)
         (struct-copy node-number node [type #:parent node-expr int])]
        [(node-assign? node)
         (define node-left (analyze variables (node-assign-left node)))
         (define node-right (analyze variables (node-assign-right node)))
         (define type
           (cond [(same-type? node-left node-right) (node-expr-type node-left)]
                 [(pointer-or-array? node-left)
                  (type-conversion-array-to-pointer (node-expr-type node-left))]
                 [(pointer-or-array? node-right)
                  (type-conversion-array-to-pointer (node-expr-type node-right))]
                 [else
                  (semantics-error (format "assign different type: ~a = ~a"
                                           (type-of node-left)
                                           (type-of node-right)) node)]))
         (struct-copy node-assign node
                      [type #:parent node-expr type]
                      [left node-left]
                      [right node-right])]
        [(node-operator? node)
         (define node-left (analyze variables (node-operator-left node)))
         (define node-right (analyze variables (node-operator-right node)))
         (define type
           (cond [(and (is-int? node-left) (is-int? node-right)) (node-expr-type node-left)]
                 [(and (is-int? node-left) (pointer-or-array? node-right))
                  (type-conversion-array-to-pointer (node-expr-type node-right))]
                 [(and (pointer-or-array? node-left) (is-int? node-right))
                  (type-conversion-array-to-pointer (node-expr-type node-left))]
                 [else
                  (semantics-error (format "apply binary operator to different types: ~a ~a ~a"
                                           (type-of node-left)
                                           (node-operator-op node)
                                           (type-of node-right))
                                   node)]))
         (struct-copy node-operator node
                      [type #:parent node-expr type]
                      [left node-left]
                      [right node-right])]
        [(node-addr? node)
         (define unary0 (analyze variables (node-unary-operator-node node)))
         (define ty (pointer-of (node-expr-type unary0)))
         (struct-copy node-unary-operator node
                      [type #:parent node-expr ty]
                      [node unary0])]
        [(node-deref? node)
         (define unary0 (analyze variables (node-unary-operator-node node)))
         (unless (pointer-or-array? unary0)
           (semantics-error "dereferencing scalar type" unary0))
         (struct-copy node-unary-operator node
                      [type #:parent node-expr (base-type unary0)]
                      [node unary0])]
        [(node-sizeof? node)
         (define unary0 (analyze variables (node-unary-operator-node node)))
         (node-number
          int
          (cond [(is-pointer? unary0) 8]
                [(is-int? unary0) 8]
                [else (semantics-error (format "unkown type:~a" (type-of unary0)) unary0)]))]
        [(node-if? node)
         (struct-copy node-if node
                      [conditional (analyze variables (node-if-conditional node))]
                      [true-clause (analyze variables (node-if-true-clause node))]
                      [false-clause (analyze variables (node-if-false-clause node))])]
        [(node-for? node)
         (struct-copy node-for node
                      [init (analyze variables (node-for-init node))]
                      [conditional (analyze variables (node-for-conditional node))]
                      [next (analyze variables (node-for-next node))]
                      [body (analyze variables (node-for-body node))])]
        [(node-while? node)
         (struct-copy node-while node
                      [conditional (analyze variables (node-while-conditional node))]
                      [body (analyze variables (node-while-body node))])]
        [(node-block? node)
         (struct-copy node-block node
                      [bodys (map (curry analyze variables) (node-block-bodys node))])]
        [(node-func-call? node)
         (struct-copy node-func-call node
                      [type #:parent node-expr int]
                      [args (map (lambda (arg) (analyze variables arg)) (node-func-call-args node))])]
        [(node-return? node)
         (struct-copy node-return node [expr (analyze variables (node-return-expr node))])]
        [(node-variable-declaration? node) node]
        [else
         (semantics-error node "unknown node")]))

(define (semantics variables nodes)
  (map
   (lambda (node)
     (define body (node-func-declaration-body node))
     (define vars (node-func-declaration-variables node))
     (struct-copy node-func-declaration node
                  [body (analyze vars body)]))
   nodes))

(module+ test
  (require rackunit)

  (define variables
    (make-hash `(("x" . ,(variable "x" int 8 0))
                 ("y" . ,(variable "y" (pointer-of (pointer-of int)) 16 0))
                 ("z" . ,(variable "z" (pointer-of int) 24 0))
                 ("a" . ,(variable "a" int 32 10)))))


  (define x (node-local-variable int "x"))
  (define y (node-local-variable (pointer-of (pointer-of int)) "y"))
  (define z (node-local-variable (pointer-of int) "z"))
  (define a (node-local-variable (array-of int) "a"))

  (define (number n) (node-number int n))

  (test-equal? "node-number"
               (analyze variables (node-number void 3))
               (number 3))

  (test-equal? "local variable: int"
               (analyze variables (node-local-variable void "x"))
               x)

  (test-equal? "local variable: pointer int"
               (analyze variables (node-local-variable void "z"))
               z)

  (test-equal? "local variable: array type"
               (analyze variables (node-local-variable void "a"))
               a)

  (test-equal? "node-assign"
               (analyze variables (node-assign void (node-local-variable void "x") (node-number void 4)))
               (node-assign int x (number 4)))

  (test-equal? "node-func-call"
               (analyze variables (node-func-call void "test" '()))
               (node-func-call int "test" '()))

  (test-equal? "node-binary-operator with same type"
               (analyze variables (node-add
                                   (node-number void 3)
                                   (node-number void 4)))
               (node-operator int "+" (number 3) (number 4)))

  (test-equal? "node-binary-operator with different type1"
               (analyze variables (node-add
                                   (node-local-variable void "y")
                                   (node-number void 4)))
               (node-operator (pointer-of (pointer-of int)) "+" y (number 4)))

  (test-equal? "node-binary-operator with different type2"
               (analyze variables (node-sub
                                   (node-number void 4)
                                   (node-local-variable void "y")))
               (node-operator (pointer-of (pointer-of int)) "-" (number 4) y))

  (test-equal? "node-addr"
               (analyze variables (node-addr (node-number void 3)))
               (node-unary-operator (pointer-of int) "&" (number 3)))

  (test-equal? "node-deref"
               (analyze variables (node-deref (node-local-variable void "y")))
               (node-unary-operator (pointer-of int) "*" y))

  (test-equal? "node-sizeof int"
               (analyze variables (node-sizeof (node-local-variable void "x")))
               (node-number int 8))

  (test-equal? "node-sizeof pointer"
               (analyze variables (node-sizeof (node-local-variable void "y")))
               (node-number int 8))

  (test-equal? "node-func-call"
               (analyze variables (node-func-call void "test" (list (node-number void 3))))
               (node-func-call int "test" (list (number 3))))

  (test-equal? "type conversion array type to pointer type(deref)"
               (analyze variables (node-deref (node-local-variable void "a")))
               (node-unary-operator int "*" a))

  (test-equal? "type conversion array type to pointer type(operator)"
               (analyze variables (node-add (node-local-variable void "a")
                                            (node-number void 3)))
               (node-operator (pointer-of int) "+"
                              a (node-number int 3)))

  (test-equal? "type conversion array type to pointer type(assign pointer = array)"
               (analyze variables (node-assign void
                                               (node-local-variable void "z")
                                               (node-local-variable void "a")))
               (node-assign (pointer-of int) z a))

  (test-equal? "type conversion array type to pointer type(assing array = pointer)"
               (analyze variables (node-assign void
                                               (node-local-variable void "a")
                                               (node-local-variable void "z")))
               (node-assign (pointer-of int) a z))

  (define tests
    (list (node-assign void (node-number void 3)
                       (node-local-variable void "y"))
          (node-deref (node-number void 3))
          (node-mul (node-local-variable void "y")
                    (node-local-variable void "z"))))

  (for-each
   (lambda (test) (test-exn "semantics error"
                       #rx"type-error"
                       (lambda () (analyze variables test))))
   tests))
