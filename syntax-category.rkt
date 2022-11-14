#lang racket/base

(provide prop:not-racket-syntax
         not-racket-syntax?)

; Used to indicate that a value in the expander environment should not be used as racket syntax,
; even though it may implement prop:procedure. In this case, prop:procedure is only used to raise a syntax
; error when used in a racket expression.
(define-values
  (prop:not-racket-syntax not-racket-syntax? not-racket-syntax-ref)
  (make-struct-type-property 'not-racket-syntax))
