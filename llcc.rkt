#lang racket

(require racket/cmdline)

(define return-value
  (command-line
   #:args (value)
   value))

(displayln ".intel_syntax noprefix")
(displayln ".global main")
(displayln "main:")
(displayln (format "\tmov rax, ~a" return-value))
(displayln "\tret")

(module+ test
  (require rackunit))
