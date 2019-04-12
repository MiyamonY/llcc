#!/usr/bin/env racket
;; File:  main.rkt
;; Author: ymiyamoto
;;
;; Created on Fri Apr 12 16:20:51 2019
;;
#lang racket
(require racket/cmdline)

(define num-to-compile
  (command-line
   #:args (num)
  num))


(define (main)
  (define num num-to-compile)

  (displayln ".intel_syntax noprefix")
  (displayln ".global main")
  (displayln "main:")
  (displayln (format "  mov rax, ~a" num))
  (displayln "  ret"))

(main)

(module+ test
  (require rackunit)
)
