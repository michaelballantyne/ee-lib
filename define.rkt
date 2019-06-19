#lang racket/base

(require
  (for-syntax racket/base syntax/parse))

(provide define-literal-forms)

(define-syntax-rule (define-literal-forms literal-set-name msg (name ...))
  (begin
    (define-for-syntax (expand-to-error stx)
      (raise-syntax-error #f msg stx))
    (define-syntax name expand-to-error)
    ...
    (begin-for-syntax
      (define-literal-set literal-set-name
        (name ...)))))