#lang racket/base

(require
  (for-syntax
   racket/base
   (rename-in syntax/parse [define/syntax-parse def/stx])
   racket/syntax
   racket/generic
   ee-lib))

(provide
 define-literal-forms
 define-extensible-syntax)

(begin-for-syntax
  (define-syntax-class symbol
    (pattern stx:id
             #:attr sym (syntax-e (attribute stx)))))

(define-syntax define-literal-forms
  (syntax-parser
    [(_ literal-set-name:id
        (~optional (~seq #:syntax-class syntax-class-name:id))
        (~optional (~seq #:binding-space (~or space:symbol #f)))
        msg:expr (name:id ...))
     #:with (spaced-name ...) (map (in-space (attribute space.sym)) (attribute name))
     #'(begin
         (define-for-syntax (expand-to-error stx)
           (raise-syntax-error #f msg stx))
         (define-syntax spaced-name expand-to-error)
         ...
         (begin-for-syntax
           (define-literal-set literal-set-name
             (spaced-name ...))
           (~? (define-syntax-class syntax-class-name
                 #:literal-sets (literal-set-name)
                 (pattern (~or spaced-name ...)))
               (begin))))]))

(require (for-syntax syntax/parse/private/sc))

(begin-for-syntax
  (define (make-extension-definition-transformer rep-constructor)
    (syntax-parser
      [(_ name:id rhs)
       #`(define-syntax name (#,rep-constructor rhs))]))

  (define-syntax-class head
    (pattern (name:id . rest)
             #:attr pat #'((~var name id) . rest))
    (pattern name:id
             #:attr pat #'(~var name id)))

  (define (make-simple-macro-definition-transformer define-form)
    (syntax-parser
      [(_ h:head . body)
       #`(#,define-form h.name
                        (syntax-parser/template
                         #,((make-syntax-introducer) this-syntax)
                         [h.pat . body]))])))

(define-syntax define-extensible-syntax
  (syntax-parser
    [(_ name)
     (def/stx gen-name (format-id #'name "gen:~a" #'name))
     (def/stx name-transform (format-id #'name "~a-transform" #'name))
     (def/stx name-rep (format-id #'name "~a-rep" #'name))
     (def/stx name-rep-procedure (format-id #'name "~a-rep-procedure" #'name))
     (def/stx define-name (format-id #'name "define-~a" #'name))
     (def/stx define-simple-name (format-id #'name "define-simple-~a" #'name))
     #'(begin
         (begin-for-syntax
           (define-generics name
             (name-transform name stx))
           (struct name-rep (procedure)
             #:methods gen-name
             [(define (name-transform s stx)
                ((name-rep-procedure s) stx))]))
         (define-syntax define-name (make-extension-definition-transformer #'name-rep))
         (define-syntax define-simple-name
           (make-simple-macro-definition-transformer #'define-name)))]))