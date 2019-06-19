#lang racket

(require (for-syntax ee-lib))

(begin-for-syntax
  (define/hygienic (my-expand stx) #:definition
    (displayln (syntax-local-context))))

(define-syntax (m stx)
  (my-expand #'foo))

m