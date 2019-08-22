#lang racket

(require ee-lib/define (for-syntax syntax/parse ee-lib))

(define-literal-forms ph-literals
  "can only be used in phone-numbers syntax"
  (number
   number-list))

(define-extensible-syntax phone-number-syntax)

(define-syntax (phone-numbers stx)
  (define/hygienic (expand+extract-number stx) #:expression
    (syntax-parse stx
      #:literals (number number-list)
      [(number s:string)
       (list #'s)]
      [(number-list num ...)
       (apply append (map expand+extract-number (syntax->list #'(num ...))))]
      [(head . rest)
       #:do [(define v (lookup #'head))]
       #:when (phone-number-syntax? v)
       (expand+extract-number
        (phone-number-syntax-transform v this-syntax))]))
  
  (syntax-parse stx
    [(_ num ...)
     (define expanded-nums
       (apply append (map expand+extract-number (syntax->list #'(num ...)))))
     #`(list #,@expanded-nums)]))

(phone-numbers
 (number "123")
 (number-list (number "123")))

(define-phone-number-syntax add-prefix
  (syntax-parser
    [(_ prefix ((~literal number) str) ...)
     #:with (prefixed ...) (map (λ (s)
                                  (datum->syntax
                                   this-syntax
                                   (string-append (syntax-e #'prefix)
                                                  (syntax-e s))))
                                (attribute str))
     #'(number-list (number prefixed) ...)]))

(phone-numbers
 (add-prefix "555"
             (number "1212")
             (number "2121"))
 (number "1234"))