#lang racket

(require "parser.rkt")
(require "node.rkt")
(require "variables.rkt")
(require "semantics.rkt")

(provide generate)

(struct instruction ()
  #:transparent)

(struct instruction-command instruction (command)
  #:transparent)

(struct instruction-label instruction (label)
  #:transparent)

(define (format-instructions instructions)
  (map (lambda (instruction)
         (cond
           [(instruction-label? instruction)
            (instruction-label-label instruction)]
           [(instruction-command? instruction)
            (format "\t~a" (instruction-command-command instruction))]
           [else
            (generate-error (format "undefined type: ~a@~a" instruction instructions))]))
       instructions))

(define (generate-error msg)
  (raise-user-error
   'generate-error "~a\n" msg))

(define (push num)
  (list (instruction-command (format "push ~a" num))))

(define (reserve-local-variables variables)
  (list (instruction-command "push rbp")
        (instruction-command "mov rbp, rsp")
        (instruction-command (format "sub rsp, ~a" (* 8 (hash-count variables))))))

(define (transfer-arguments args variables)
  (define registers '("r9" "r8" "rcx" "rdx" "rsi" "rdi"))
  (append-map
   (lambda (arg reg)
     (define name (node-local-variable-name arg))
     (define offset (variable-offset (find-variable variables name)))
     (list (instruction-command "mov rax, rbp")
           (instruction-command (format "sub rax, ~a" offset))
           (instruction-command (format "mov [rax], ~a" reg))))
   (reverse args)
   (take-right registers (length args))))

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

(define (deref)
  (list (instruction-command "pop rax")
        (instruction-command "mov rax, [rax]")
        (instruction-command "push rax")))

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

(define (generate-left-value variables node)
  (cond [(node-local-variable? node)
         (define name (node-local-variable-name node))
         (define offset (variable-offset (find-variable variables name)))
         (list (instruction-command "mov rax, rbp")
               (instruction-command (format "sub rax, ~a" offset))
               (instruction-command "push rax"))]
        [(node-deref? node)
         (generate-node variables (node-unary-operator-unary node))]
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
             (append (generate-left-value variables node)
                     (generate-load-from-local-variable))]
            [(node-assign? node)
             (append (generate-left-value variables (node-assign-left node))
                     (generate-node variables (node-assign-right node))
                     (generate-store-to-local-variable))]
            [(node-return? node)
             (append (generate-node variables (node-return-expr node))
                     (pop-result)
                     (free-local-variables)
                     (return))]
            [(node-if? node)
             (define label-else (gen-label))
             (define label-end (gen-label))
             (append (generate-node variables (node-if-conditional node))
                     (generate-equal-0)
                     (generate-jump-if-equal label-else)
                     (generate-node variables (node-if-true-clause node))
                     (generate-jump label-end)
                     (generate-label label-else)
                     (generate-node variables (node-if-false-clause node))
                     (generate-label label-end))]
            [(node-while? node)
             (define label-start (gen-label))
             (define label-end (gen-label))
             (append (generate-label label-start)
                     (generate-node variables (node-while-conditional node))
                     (generate-equal-0)
                     (generate-jump-if-equal label-end)
                     (generate-node variables (node-while-body node))
                     (generate-jump label-start)
                     (generate-label label-end))]
            [(node-for? node)
             (define label-start (gen-label))
             (define label-end (gen-label))
             (append (generate-node variables (node-for-init node))
                     (pop-result)
                     (generate-label label-start)
                     (generate-node variables (node-for-conditional node))
                     (generate-equal-0)
                     (generate-jump-if-equal label-end)
                     (generate-node variables (node-for-body node))
                     (generate-node variables (node-for-next node))
                     (generate-jump label-start)
                     (generate-label label-end))]
            [(node-block? node)
             (append-map (curry generate-node variables) (node-block-bodys node))]
            [(node-func-call? node)
             (define func (node-func-call-func node))
             (define args (node-func-call-args node))
             (cond [(null? args) (generate-func-call func)]
                   [else
                    (append (append-map (curry generate-node variables) args)
                            (generate-func-call-with-args func (length args)))])]
            [(node-operator? node)
             (append (generate-node variables (node-operator-left node))
                     (generate-node variables (node-operator-right node))
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
            [(node-addr? node)
             (append (generate-left-value variables (node-unary-operator-unary node)))]
            [(node-deref? node)
             (append (generate-node variables (node-unary-operator-unary node))
                     (deref))]
            [(node-func-declaration? node)
             (define name (node-func-declaration-name node))
             (define args (node-func-declaration-args node))
             (define body (node-func-declaration-body node))
             (define variables (node-func-declaration-variables node))
             (append (generate-label name)
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
