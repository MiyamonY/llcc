#lang racket

(require "parser.rkt")
(require "node.rkt")
(require "variables.rkt")
(require "semantics.rkt")
(require "type.rkt")

(provide generate)

(struct instruction ()
  #:transparent)

(struct command instruction (operation)
  #:transparent)

(struct label instruction (name)
  #:transparent)

(struct comment instruction (message)
  #:transparent)

(define (format-instructions instructions)
  (map (lambda (instruction)
         (cond
           [(label? instruction)
            (label-name instruction)]
           [(command? instruction)
            (format "\t~a" (command-operation instruction))]
           [(comment? instruction)
            (format "# ~a" (comment-message instruction))]
           [else
            (generate-error (format "undefined type: ~a@~a" instruction instructions))]))
       instructions))

(define (generate-error msg)
  (raise-user-error
   'generate-error "~a\n" msg))

(define (push num)
  (list (command (format "push ~a" num))))

(define (reserve-local-variables variables)
  (list (command "push rbp")
        (command "mov rbp, rsp")
        (command (format "sub rsp, ~a" (* 8 (hash-count variables))))))

(define (transfer-arguments args variables)
  (define registers '("r9" "r8" "rcx" "rdx" "rsi" "rdi"))
  (append-map
   (lambda (arg reg)
     (define name (node-local-variable-name arg))
     (define offset (variable-offset (find-variable variables name)))
     (list (command "mov rax, rbp")
           (command (format "sub rax, ~a" offset))
           (command (format "mov [rax], ~a" reg))))
   (reverse args)
   (take-right registers (length args))))

(define (free-local-variables)
  (list (command "mov rsp, rbp")
        (command "pop rbp")))

(define (push-result)
  (list (command "push rax")))

(define (push-variable variables name)
  (define offset (variable-offset (find-variable variables name)))
  (list (command "mov rax, rbp")
        (command (format "sub rax, ~a" offset))
        (command "push rax")))

(define (pop-result)
  (list (command "pop rax")))

(define (pop-operands)
  (list (command "pop rdi")
        (command "pop rax")))

(define (return)
  (list (command "ret")))

(define (deref)
  (list (command "pop rax")
        (command "mov rax, [rax]")
        (command "push rax")))

(define (add)
  (list (command "add rax, rdi")))

(define (sub)
  (list (command "sub rax, rdi")))

(define (mul)
  (list (command "imul rax, rdi")))

(define (div)
  (list (command "cqo")
        (command "div rdi")))

(define (eq)
  (list (command "cmp rax, rdi")
        (command "sete al")
        (command "movzb rax, al")))

(define (neq)
  (list (command "cmp rax, rdi")
        (command "setne al")
        (command "movzb rax, al")))

(define (lt)
  (list (command "cmp rax, rdi")
        (command "setl al")
        (command "movzb rax, al")))

(define (le)
  (list (command "cmp rax, rdi")
        (command "setle al")
        (command "movzb rax, al")))

(define (equal0)
  (list (command  "pop rax")
        (command  "cmp rax, 0")))

(define (jump-if-equal to)
  (list (command (format "je ~a" to))))

(define (jump to)
  (list (command (format "jmp ~a" to))))

(define (load-from-local-variable variables var)
  (if (array? (variable-type (find-variable variables var)))
      (list (command "pop rax")
            (command "push rax"))
      (list (command "pop rax")
            (command "mov rax, [rax]")
            (command "push rax"))))

(define (store-to-local-variable)
  (list (command "pop rdi")
        (command "pop rax")
        (command "mov [rax],rdi")
        (command "push rdi")))

(define (label: name)
  (list (label (format "~a:" name))))

(define (func-call func)
  (list (command (format "call ~a" func))
        (command "push rax")))

(define (func-call-with-args func arg-num)
  (define registers '("r9" "r8" "rcx" "rdx" "rsi" "rdi"))
  (append (append-map (lambda (reg)
                        (list (command "pop rax")
                              (command (format "mov ~a, rax" reg))))
                      (take-right registers arg-num))
          (func-call func)))

(define (left-value variables node)
  (cond [(node-local-variable? node)
         (define name (node-local-variable-name node))
         (push-variable variables name)]
        [(node-deref? node)
         (append (list (comment "node left value"))
                 (generate-node variables (node-unary-operator-node node)))]
        [else
         (generate-error
          (format "left value must be local variable or deref operator: ~a" (object-name node)))]))

(define gen-label
  ((lambda ()
     (define label-num 0)
     (lambda ()
       (set! label-num (add1 label-num))
       (format ".Llabel~a" label-num)))))

(define (generate-node variables node)
  (if (null? node)
      '()
      (cond [(node-number? node) (push (node-number-val node))]
            [(node-local-variable? node)
             (append (left-value variables node)
                     (load-from-local-variable variables (node-local-variable-name node)))]
            [(node-assign? node)
             (append (left-value variables (node-assign-left node))
                     (generate-node variables (node-assign-right node))
                     (store-to-local-variable))]
            [(node-return? node)
             (append (generate-node variables (node-return-expr node))
                     (pop-result)
                     (free-local-variables)
                     (return))]
            [(node-if? node)
             (define label-else (gen-label))
             (define label-end (gen-label))
             (append (generate-node variables (node-if-conditional node))
                     (equal0)
                     (jump-if-equal label-else)
                     (generate-node variables (node-if-true-clause node))
                     (jump label-end)
                     (label: label-else)
                     (generate-node variables (node-if-false-clause node))
                     (label: label-end))]
            [(node-while? node)
             (define label-start (gen-label))
             (define label-end (gen-label))
             (append (label: label-start)
                     (generate-node variables (node-while-conditional node))
                     (equal0)
                     (jump-if-equal label-end)
                     (generate-node variables (node-while-body node))
                     (jump label-start)
                     (label: label-end))]
            [(node-for? node)
             (define label-start (gen-label))
             (define label-end (gen-label))
             (append (generate-node variables (node-for-init node))
                     (pop-result)
                     (label: label-start)
                     (generate-node variables (node-for-conditional node))
                     (equal0)
                     (jump-if-equal label-end)
                     (generate-node variables (node-for-body node))
                     (generate-node variables (node-for-next node))
                     (jump label-start)
                     (label: label-end))]
            [(node-block? node)
             (append-map (curry generate-node variables) (node-block-bodys node))]
            [(node-func-call? node)
             (define func (node-func-call-func node))
             (define args (node-func-call-args node))
             (cond [(null? args) (func-call func)]
                   [else
                    (append (append-map (curry generate-node variables) args)
                            (func-call-with-args func (length args)))])]
            [(node-operator? node)
             (define left (node-operator-left node))
             (define right (node-operator-right node))
             (append (list (comment "node operator"))
                     (generate-node variables left)
                     (if (and (int? left) (pointer-or-array? right))
                         (append
                          (push (if (pointer? (base-type right)) 8 8))
                          (pop-operands)
                          (mul)
                          (push-result))
                         '())
                     (generate-node variables right)
                     (if (and (pointer-or-array? left) (int? right))
                         (append
                          (push (if (pointer? (base-type left)) 8 8))
                          (pop-operands)
                          (mul)
                          (push-result))
                         '())
                     (pop-operands)
                     (cond [(node-add? node) (add)]
                           [(node-sub? node) (sub)]
                           [(node-mul? node) (mul)]
                           [(node-div? node) (div)]
                           [(node-eq? node) (eq)]
                           [(node-neq? node) (neq)]
                           [(node-lt? node) (lt)]
                           [(node-le? node) (le)]
                           [else
                            (generate-error (format "unexpected operator: ~a" (node-operator-op node)))])
                     (push-result))]
            [(node-addr? node)
             (append (left-value variables (node-unary-operator-node node)))]
            [(node-deref? node)
             (append (generate-node variables (node-unary-operator-node node))
                     (deref))]
            [(node-func-declaration? node)
             (define name (node-func-declaration-name node))
             (define args (node-func-declaration-args node))
             (define body (node-func-declaration-body node))
             (define variables (node-func-declaration-variables node))
             (append (label: name)
                     (reserve-local-variables variables)
                     (transfer-arguments args variables)
                     (generate-node variables body)
                     (pop-result)
                     (free-local-variables)
                     (return))]
            [(node-variable-declaration? node) '()])))

(define (generate input)
  (define-values (variables nodes) (parse input))
  (define nodes0 (semantics variables nodes))
  (define body (append-map (curry generate-node variables) nodes0))

  (string-join
   `(".intel_syntax noprefix"
     ".global main"
     ,@(format-instructions body))
   "\n"))

(module+ test
  (require rackunit))
