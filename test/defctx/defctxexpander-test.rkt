#lang racket


(require "defctxexpander.rkt" (for-syntax syntax/parse))

(define-syntax (m stx)
  (syntax-parse stx
    [(_)
     #'x]))

(#%expression
 (my-l
  (my-λ (x) (m))))
