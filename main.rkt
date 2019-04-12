;;;
;; File:  main.rkt
;; Author: ymiyamoto
;;
;; Created on Fri Apr 12 16:20:51 2019
;;
#!/usr/bin/env racket
#lang racket
(require racket/cmdline)
(require parser)

(define arith-to-compile
  (command-line
   #:args (arith)
  arith))

(define (main)
  (define arith arith-to-compile)
  (define parsed (parser-arith arith))

  (displayln ".intel_syntax noprefix")
  (displayln ".global main")
  (displayln "main:")
  (displayln (format "  mov rax, ~v" (car arith)))
  (displayln "  ret"))
