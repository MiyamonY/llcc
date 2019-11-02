#lang racket

(require "generator.rkt")

(define (compile input)
  (generate input))

(module+ main
  (require racket/cmdline)

  (define input (command-line #:args (input) input))
  (displayln (compile input)))
