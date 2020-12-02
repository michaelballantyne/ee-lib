#lang racket

(require (for-syntax syntax/parse "../effect-trampoline.rkt"))

(define-syntax test-def
  (effect-trampoline-macro
   (syntax-parser
     [(_ name:id rhs:expr)
      (trampoline-lift! #'(define-syntax name rhs))
      (define val (syntax-local-value #'name))
      (displayln (val #'(_ 5)))
      #'(name 5)])))

(test-def foo (syntax-parser [(_ arg) #'(+ arg 1)]))