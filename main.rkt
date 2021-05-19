#lang racket/base

(require
  syntax/apply-transformer
  racket/syntax
  syntax/parse
  syntax/parse/define

  syntax/id-table
  (for-template "private/lift-disappeareds.rkt")
  (for-syntax
   racket/base
   syntax/parse
   racket/syntax
   (only-in syntax/parse [define/syntax-parse def/stx]))
  (for-template racket/base))

(provide
 qstx/rc ; read as quasisyntax/loc+props
 qstx/lp

 bind!
 racket-var
 racket-var?
 with-scope
 scope-tagger?
 scope-tagger-introducer
 add-scope
 splice-from-scope
 add-scopes
 lookup
 apply-as-transformer
 define/hygienic
 define/hygienic-metafunction
 current-def-ctx
 current-ctx-id

 eval-transformer

 map-transform
 syntax-local-introduce-splice
 add-fresh-name!

 module-macro
 non-module-begin-macro
 expression-macro
 definition-macro
 )

(define-syntax (qstx/lp stx)
  (syntax-case stx ()
    [(_ arg template)
     #`(let ([orig arg])
         (datum->syntax (quote-syntax #,stx)
                        (syntax-e (quasisyntax template))
                        orig orig))]))

(define-syntax (qstx/rc stx)
  (syntax-case stx ()
    [(_ template)
     #`(datum->syntax (quote-syntax #,stx)
                      (syntax-e (quasisyntax template))
                      this-syntax this-syntax)]))


(define current-def-ctx (make-parameter #f))
(define current-ctx-id (make-parameter #f))

(struct scope-tagger [introducer def-ctx])

(define (call-with-scope p)
  (let* ([ctx (syntax-local-make-definition-context (current-def-ctx))]
         [sc (scope-tagger (make-syntax-introducer #t) ctx)])
    (parameterize ([current-def-ctx (scope-tagger-def-ctx sc)]
                   [current-ctx-id (gensym 'with-scope-ctx)])
      (p sc))))

(define-simple-macro
  (with-scope name:id body ...)
  (call-with-scope (lambda (name) body ...)))

(define (add-scope stx sc)
  (unless (syntax? stx)
    (raise-argument-error
     'add-scope
     "syntax?"
     stx))
  (unless (scope-tagger? sc)
    (raise-argument-error
     'add-scope
     "scope-tagger?"
     sc))
  (internal-definition-context-introduce
    (scope-tagger-def-ctx sc)
    ((scope-tagger-introducer sc) stx 'add) 'add))

(define (add-scopes stx scs)
  (unless (syntax? stx)
    (raise-argument-error
     'add-scopes
     "syntax?"
     stx))
  (unless (and (list? scs) (andmap scope-tagger? scs))
    (raise-argument-error
     'add-scopes
     "(listof scope-tagger?)"
     scs))
  
  (for/fold ([stx stx])
            ([sc scs])
    ((scope-tagger-introducer sc) stx 'add)))

(define (splice-from-scope stx sc)
  (unless (syntax? stx)
    (raise-argument-error
     'remove-scope
     "syntax?"
     stx))
  (unless (scope-tagger? sc)
    (raise-argument-error
     'remove-scope
     "scope-tagger?"
     sc))
  (internal-definition-context-introduce
   (scope-tagger-def-ctx sc)
   ((scope-tagger-introducer sc) stx 'remove)
   'remove))

(define (add-ctx-scope ctx stx)
  (if ctx
    (internal-definition-context-introduce ctx stx 'add)
    stx))

(struct racket-var [])

(define (bind! id rhs-arg)
  (unless (or (identifier? id) (and (list? id) (andmap identifier? id)))
    (raise-argument-error
     'bind!
     "(or/c identifier? (listof identifier?))"
     id))
  (unless (current-ctx-id)
    (error 'bind!
           "cannot bind outside of dynamic extent of with-scope"))
  (unless (current-def-ctx)
    (error 'bind!
           "cannot bind in outer scope from an expression context"))
  (when (not rhs-arg)
    (error 'bind! "environment value must not be #f"))

  (define rhs
    (if (racket-var? rhs-arg)
        #f
        #`'#,rhs-arg))

  (syntax-local-bind-syntaxes (if (list? id) id (list id)) rhs (current-def-ctx))

  (define (in-sc id)
    (add-ctx-scope (current-def-ctx) (syntax-local-identifier-as-binding id)))

  (define id-in-sc (if (list? id) (map in-sc id) (in-sc id)))
  (apply lift-disappeared-bindings! (if (list? id-in-sc) id-in-sc (list id-in-sc)))
  id-in-sc)

; This should use syntax-local-eval, but that's currently buggy.
; Change to use syntax-local-eval after my fix gets in a release:
; https://github.com/racket/racket/pull/3517
(define (eval-transformer stx)
  (define ctx (syntax-local-make-definition-context (current-def-ctx)))
  (define id (generate-temporary #'x))

  (syntax-local-bind-syntaxes
   (list id)
   (add-ctx-scope (current-def-ctx) stx)
   ctx)

  (syntax-local-value
   (internal-definition-context-introduce ctx id 'add)
   (lambda () (error 'eval-transformer "shouldn't happen"))
   ctx))

; used only for eq? equality.
(define unbound
  (let ()
    (struct unbound [])
    (unbound)))

(define (lookup id [predicate (lambda (v) #t)])
  (unless (identifier? id)
    (raise-argument-error
     'lookup
     "identifier?"
     id))

  (define id-in-sc (add-ctx-scope (current-def-ctx) id))
  (define result
    (syntax-local-value
     id-in-sc
     (lambda () unbound)
     (current-def-ctx)))

  (when (not result)
    (error 'lookup "invariant violated; got #f from environment lookup"))

  (if (and (not (eq? result unbound)) (predicate result))
      (begin
        (lift-disappeared-uses! id-in-sc)
        result)
      #f))

(define (syntax-local-introduce-splice stx)
  (syntax-local-identifier-as-binding
   (syntax-local-introduce stx)))

(define (apply-as-transformer f f-id ctx-type-arg . args)
  (unless (procedure? f)
    (raise-argument-error
     'apply-as-transformer
     "procedure?"
     f))
  (unless (member ctx-type-arg '(expression definition))
    (raise-argument-error
     'apply-as-transformer
     "(or/c 'expression 'definition)"
     ctx-type-arg))

  (apply-with-hygiene f f-id ctx-type-arg #t args))

(define (syntax-local-apply-transformer-use-site-workaround
         f f-id ctx-type def-ctx . args)
  (if (eq? ctx-type 'expression)
      ; Expand as a definition first to get a use-site scope, as a workaround for
      ; https://github.com/racket/racket/pull/2237
      (apply syntax-local-apply-transformer
             (lambda args
               (apply syntax-local-apply-transformer f f-id 'expression def-ctx args))
             f-id (list (gensym)) def-ctx args)
      (apply syntax-local-apply-transformer f f-id ctx-type def-ctx args)))

(define (apply-with-hygiene f f-id ctx-type seal? args)
  (define def-ctx (current-def-ctx))
  (parameterize ([current-def-ctx (if seal? #f (current-def-ctx))])
    (apply syntax-local-apply-transformer-use-site-workaround
           f
           f-id
           (case ctx-type
             [(expression) 'expression]
             [(definition) (list (current-ctx-id))])
           def-ctx
           args)))

(begin-for-syntax
  (define-syntax-class ctx-type
    (pattern #:expression
             #:attr type #''expression
             #:attr seal? #'#t)
    (pattern #:definition
             #:attr type #''definition
             #:attr seal? #'#f)))

(define-syntax define/hygienic
  (syntax-parser
    [(_ (name:id arg:id ...) ctx:ctx-type
        body ...+)
     #'(begin
         (define (tmp arg ...)
           body ...)
         (define (name arg ...)
           (apply-with-hygiene tmp #'name ctx.type ctx.seal? (list arg ...))))]))

; convenient for cmdline-ee case study
(require syntax/parse/experimental/template)
(provide define/hygienic-metafunction)
(define-syntax define/hygienic-metafunction
  (syntax-parser
    [(_ (name:id arg:id) ctx:ctx-type
        body ...)
     #'(begin
         (define (tmp arg)
           body ...)
         (define-template-metafunction (name stx)
           (syntax-parse stx
             [(_ t)
              (apply-as-transformer tmp #'name ctx.type #'t)])))]))

; applies the function f to each element of the tree, starting
; from the leaves. For nodes wrapped as a syntax object, the function
; is applied to the syntax object but not its immediate datum contents.
(define (map-transform f stx)
  (define (recur stx)
    (cond
      [(syntax? stx)
       (let ([e (syntax-e stx)])
         (datum->syntax stx (recur e) stx stx))]
      [(pair? stx)
       (cons
        (map-transform f (car stx))
        (map-transform f (cdr stx)))]
      ; TODO: handle vectors and other composite data that may appear in syntax
      [else stx]))
  (f (recur stx)))

; Do I use this? Should it be a persistent-id-table operation instead?

(define (add-fresh-name! table id)
  (unless (mutable-free-id-table? table)
    (raise-argument-error
     'add-fresh-name!
     "mutable-free-id-table?"
     table))
  (unless (identifier? id)
    (raise-argument-error
     'add-fresh-name!
     "identifier?"
     id))
    
  (define result (generate-temporary id))

  (free-id-table-set! table
                      (syntax-local-introduce id)
                      result)
  
  (syntax-local-introduce
   result))

(define (module-macro t)
  (lambda (stx)
    (case (syntax-local-context)
      [(module-begin) #`(begin #,stx)]
      [(module) (t stx)]
      [else (raise-syntax-error #f "Only allowed in module context" stx)])))

(define (non-module-begin-macro t)
  (lambda (stx)
    (case (syntax-local-context)
      [(module-begin) #`(begin #,stx)]
      [else (t stx)])))

(define (definition-macro t)
  (lambda (stx)
    (case (syntax-local-context)
      [(module-begin) #`(begin #,stx)]
      [(expression) (raise-syntax-error #f "Only allowed in a definition context" stx)]
      [else (t stx)])))

(define (expression-macro t)
  (lambda (stx)
    (case (syntax-local-context)
      [(expression) (t stx)]
      [else #`(#%expression #,stx)])))
