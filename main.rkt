#!/usr/bin/env racket
#lang racket
;;;
;; File:  main.rkt
;; Author: ymiyamoto
;;
;; Created on Fri Apr 12 16:20:51 2019
;;
(require racket/cmdline)
(require "parser.rkt")
(require "assemble.rkt")

(define arith-to-compile
  (command-line
   #:args (arith)
  arith))

(define (main)
  (define arith arith-to-compile)
  (define parsed (parse arith))
  (displayln (assemble parsed)))

(main)
