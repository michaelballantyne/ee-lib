#lang racket

(provide my-λ my-l)

(require (for-syntax syntax/parse syntax/apply-transformer))

(define-syntax my-λ #f)

(begin-for-syntax
  (struct my-var [])
  (define (expand-l e)
    (syntax-parse e
      #:literals (my-λ)
      [x:id #:when (my-var? (syntax-local-value #'x (lambda () #f)))
            e]
      [(my-λ (x:id) b:expr)
       (define ctx (syntax-local-make-definition-context))
       (syntax-local-bind-syntaxes (list #'x) #'(my-var) ctx)
       (define x^ (internal-definition-context-introduce ctx #'x 'add))
       (displayln (syntax-debug-info (syntax-local-introduce x^)))
       (define b^ (local-apply-transformer expand-l #'b 'expression (list ctx)))
       #`(my-λ (#,x^) #,b^)]
      [(e1:expr e2:expr)
       (define e1^ (local-apply-transformer expand-l #'e1 'expression))
       (define e2^ (local-apply-transformer expand-l #'e2 'expression))
       #`(#,e1^ #,e2^)]
      [(m:id . rest)
       (define t (syntax-local-value #'m))
       (expand-l (local-apply-transformer t e 'expression))])))


(define-syntax (my-l stx)
  (syntax-parse stx
    [(_ e)
     #`#'#,(expand-l #'e)]))