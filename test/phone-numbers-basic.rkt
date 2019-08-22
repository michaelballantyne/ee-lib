#lang racket

(require (for-syntax syntax/parse syntax/apply-transformer))

(define-syntax number
  (lambda (stx)
    (raise-syntax-error #f "number can only be used in phone-numbers syntax" stx)))
(define-syntax number-list
  (lambda (stx)
    (raise-syntax-error #f "number-list can only be used in phone-numbers syntax" stx)))

(begin-for-syntax
  (struct phone-number-macro (proc)
    #:property prop:procedure
    (lambda (s stx) (raise-syntax-error #f "phone number macros can only be used in phone-numbers syntax" stx))))

(define-syntax (phone-numbers stx)
  (define (expand+extract-number stx)
    (syntax-parse stx
      #:literals (number number-list)
      [(number s:string)
       (list #'s)]
      [(number-list num ...)
       (apply append (map expand+extract-number (syntax->list #'(num ...))))]
      [(head . rest)
       #:do [(define v (syntax-local-value #'head (lambda () #f)))]
       #:when (phone-number-macro? v)
       (expand+extract-number
        (local-apply-transformer (phone-number-macro-proc v)
                                 this-syntax
                                 'expression
                                 '()))]))
  
  (syntax-parse stx
    [(_ num ...)
     (define expanded-nums
       (apply append (map expand+extract-number (syntax->list #'(num ...)))))
     #`(list #,@expanded-nums)]))

(phone-numbers
 (number "123")
 (number-list (number "123")))

(define-syntax add-prefix
  (phone-number-macro
   (syntax-parser
     [(_ prefix ((~literal number) str) ...)
      #:with (prefixed ...) (map (λ (s)
                                   (datum->syntax
                                    this-syntax
                                    (string-append (syntax-e #'prefix)
                                                   (syntax-e s))))
                                 (attribute str))
      #'(number-list (number prefixed) ...)])))

(phone-numbers
 (add-prefix "555"
             (number "1212")
             (number "2121"))
 (number "1234"))