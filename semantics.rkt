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
         (define type (variable-type (find-variable variables name)))
         (struct-copy node-local-variable node [type #:parent node-expr type])]
        [(node-number? node)
         (struct-copy node-number node [type #:parent node-expr int])]
        [(node-assign? node)
         (define node-left (analyze variables (node-assign-left node)))
         (define node-right (analyze variables (node-assign-right node)))
         (unless (same-type? node-left node-right)
           (semantics-error (format "assign different type: ~a = ~a"
                                    (type-of node-left)
                                    (type-of node-right)) node))
         (struct-copy node-assign node
                      [type #:parent node-expr (node-expr-type node-right)]
                      [left node-left]
                      [right node-right])]
        [(node-operator? node)
         (define node-left (analyze variables (node-operator-left node)))
         (define node-right (analyze variables (node-operator-right node)))
         (define ty
           (cond [(and (is-int? node-left) (is-int? node-right))
                  (node-expr-type node-left)]
                 [(and (is-int? node-left) (is-pointer? node-right))
                  (node-expr-type node-right)]
                 [(and (is-pointer? node-left) (is-int? node-right))
                  (node-expr-type node-left)]
                 [else
                  (semantics-error (format "apply binary operator to different types: ~a ~a ~a"
                                           (type-of node-left)
                                           (node-operator-op node)
                                           (type-of node-right))
                                   node)]))
         (struct-copy node-operator node
                      [type #:parent node-expr ty]
                      [left node-left]
                      [right node-right])]
        [(node-addr? node)
         (define unary0 (analyze variables (node-unary-operator-unary node)))
         (define ty (pointer-of (node-expr-type unary0)))
         (struct-copy node-unary-operator node
                      [type #:parent node-expr ty]
                      [unary unary0])]
        [(node-deref? node)
         (define unary0 (analyze variables (node-unary-operator-unary node)))
         (unless (is-pointer? unary0)
           (semantics-error "dereferencing scalar type" unary0))
         (struct-copy node-unary-operator node
                      [type #:parent node-expr (base-type unary0)]
                      [unary unary0])]
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
    (make-hash `(("x" . ,(variable "x" int 8))
                 ("y" . ,(variable "y" (pointer-of (pointer-of int)) 16))
                 ("z" . ,(variable "z" (pointer-of int) 24)))))

  (define x (node-local-variable int "x"))
  (define y (node-local-variable (pointer-of (pointer-of int)) "y"))

  (define (number n) (node-number int n))

  (test-equal? "node-number"
               (analyze variables (node-number void 3))
               (number 3))

  (test-equal? "node-local-variable"
               (analyze variables (node-local-variable void "x"))
               x)

  (test-equal? "node-assign"
               (analyze variables (node-assign void (node-local-variable void "x") (node-number void 4)))
               (node-assign int x (number 4)))

  (test-equal? "node-func-call"
               (analyze variables (node-func-call void "test" '()))
               (node-func-call int "test" '()))

  (test-equal? "node-binary-operator with same type"
               (analyze variables (node-operator void "+"
                                                 (node-number void 3)
                                                 (node-number void 4)))
               (node-operator int "+" (number 3) (number 4)))

  (test-equal? "node-binary-operator with different type1"
               (analyze variables (node-operator void "+"
                                                 (node-local-variable void "y")
                                                 (node-number void 4)))
               (node-operator (pointer-of (pointer-of int)) "+" y (number 4)))

  (test-equal? "node-binary-operator with different type2"
               (analyze variables (node-operator void "-"
                                                 (node-number void 4)
                                                 (node-local-variable void "y")))
               (node-operator (pointer-of (pointer-of int)) "-" (number 4) y))

  (test-equal? "node-addr"
               (analyze variables (node-unary-operator void "&" (node-number void 3)))
               (node-unary-operator (pointer-of int) "&" (number 3)))

  (test-equal? "node-deref"
               (analyze variables (node-unary-operator void "*" (node-local-variable void "y")))
               (node-unary-operator (pointer-of int) "*" y))

  (test-equal? "node-func-call"
               (analyze variables (node-func-call void "test" (list (node-number void 3))))
               (node-func-call int "test" (list (number 3))))

  (define tests
    (list (node-assign void (node-number void 3)
                       (node-local-variable void "y"))
          (node-unary-operator void "*" (node-number void 3))
          (node-operator void "*" (node-local-variable void "y")
                         (node-local-variable void "z"))))

  (for-each
   (lambda (test) (test-exn "semantics error"
                       #rx"type-error"
                       (lambda () (analyze variables test))))
   tests))
