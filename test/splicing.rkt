#lang racket/base

(require
  ee-lib/define
  (for-syntax
   racket/base
   racket/pretty
   syntax/parse
   ee-lib
   syntax/stx
   racket/match))

(define-literal-forms spl
  "cannot be used in Racket"
  (spl-block spl-define spl-define-syntax spl-begin spl-splicing-let))

(begin-for-syntax
  (define spl-def-bind! (make-parameter #f))

  (struct spl-var [])
  (struct spl-macro [transformer])
  
  (define/hygienic (spl-expand-expr stx) #:expression
    (syntax-parse stx
      #:literal-sets (spl)
      [(spl-block body ...)
       (with-scope sc
         (let ([this-def-ctx (current-def-ctx)]
               [this-ctx-id (current-ctx-id)])
           (parameterize ([spl-def-bind!
                           (lambda (id v)
                             (parameterize ([current-def-ctx this-def-ctx]
                                            [current-ctx-id this-ctx-id])
                               (bind! id v)))])
             (define/syntax-parse (body^ ...)
               (map spl-expand-def (syntax->list (add-scope #'(body ...) sc))))
             #'(spl-block body^ ...))))]
      [x:id
       ;(pretty-print (syntax-debug-info (flip-intro-scope #'x)))
       (when (not (lookup #'x spl-var?))
         (raise-syntax-error #f "unbound reference" #'x))
       #'x]
      [n:number
       #'n]
      [(m:id . rest)
       #:do [(define binding (lookup #'m spl-macro?))]
       #:when binding
       (spl-expand-expr
        (apply-as-transformer (spl-macro-transformer binding)
                              #'m
                              'expression
                              this-syntax))]))
  
  (define/hygienic (spl-expand-def stx) #:definition
    (syntax-parse stx
      #:literal-sets (spl)
      [(spl-define x:id e:expr)
       (define/syntax-parse e^ (spl-expand-expr #'e))
       (define/syntax-parse x^ ((spl-def-bind!) #'x (spl-var)))
       #'(spl-define x^ e^)]
      [(spl-define-syntax m:id e:expr)
       (define/syntax-parse m^ ((spl-def-bind!) #'m (eval-transformer #'(spl-macro e))))
       #'(spl-define-syntax m^ e)]
      [(spl-begin body ...)
       (define/syntax-parse (body^ ...)
         (map spl-expand-def (syntax->list #'(body ...))))
       #'(spl-begin body^ ...)]
      [(spl-splicing-let ([v e]) body ...)
       (with-scope sc
         (define/syntax-parse v^ (bind! (add-scope #'v sc) (spl-var)))
         (define/syntax-parse e^ (spl-expand-expr (add-scope #'e sc)))
         (let ([outer-def-bind! (spl-def-bind!)])
           (parameterize ([spl-def-bind! (lambda (id v)
                                           (outer-def-bind! (splice-from-scope id sc) v))])
             (define/syntax-parse (body^ ...) (stx-map spl-expand-def (add-scope #'(body ...) sc)))
             #'(spl-splicing-let ([v^ e^]) body^ ...))))]
      [(m:id . rest)
       #:do [(define binding (lookup #'m spl-macro?))]
       #:when binding
       (spl-expand-def
        (apply-as-transformer (spl-macro-transformer binding)
                              #'m
                              'definition
                              this-syntax))]
      [e
       (spl-expand-expr #'e)])))

(define-syntax (spl stx)
  (syntax-parse stx
    [(_ e)
     (with-handlers ([exn:fail:syntax? (lambda (e) #''fail)])
       (spl-expand-expr #'e)
       #''success)]))

(require rackunit)

; Definitions should splice out of spl-splicing-let
(check-equal?
 (spl
  (spl-block
   (spl-splicing-let ([x 5])
     (spl-define y x))
   y))
 'success)

; Abstractions over definitions should work, including through begin
(check-equal?
 (spl
  (spl-block
   (spl-splicing-let ([x 5])
     (spl-define-syntax my-spl-define
       (syntax-parser
         [(_ a v)
          #'(spl-begin (spl-define a v))]))
     (my-spl-define y x))
   y))
 'success)

; Expansions of macros defined in the splicing context should
; apply use-site scopes.
(check-equal?
 (spl
  (spl-block
   (spl-splicing-let ([x 5])
     (spl-define-syntax m
       (syntax-parser
         [(_ a)
          #'(spl-block
             (spl-define a x)
             y)]))
     (m y))))
 'fail)

(begin-for-syntax
  (define debug-info #f))

(define-syntax (any-use-sites? stx)
  (if (ormap (lambda (sc)
               (match sc
                 [(vector num 'use-site) #t]
                 [_ #f]))
             (hash-ref debug-info 'context))
      #'#t
      #'#f))

; The frame-id optimization should avoid use-site scopes in this case
; because the use-site is expanded by a new definition context within
; an expression context relative to the definition.
(check-equal?
 (list
  (spl
   (spl-block
    (spl-define-syntax m
      (syntax-parser
        [(_ a)
         (set! debug-info (syntax-debug-info (flip-intro-scope #'a)))
         #'(spl-block
            (spl-define a x)
            y)]))
    (spl-block
     (m x))))
  (any-use-sites?))
 '(fail #f))
  
