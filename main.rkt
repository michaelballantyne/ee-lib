#lang racket/base

(require
  syntax/apply-transformer
  racket/syntax
  syntax/parse
  syntax/parse/define

  syntax/id-table
  (rename-in "private/apply-as-transformer.rkt"
             [apply-as-transformer prim-apply-as-transformer])
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
 with-scope
 scope?
 scope-introducer
 add-scope
 splice-from-scope
 add-scopes
 unbound
 lookup
 apply-as-transformer
 define/hygienic
 define/hygienic-metafunction
 current-def-ctx
 current-ctx-id

 eval-transformer

 map-transform
 syntax-local-introduce-splice
 add-fresh-name!)

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

(struct scope [introducer def-ctx])

(define (call-with-scope p)
  (let* ([ctx (syntax-local-make-definition-context (current-def-ctx))]
         [sc (scope (make-syntax-introducer #t) ctx)])
    (parameterize ([current-def-ctx (scope-def-ctx sc)]
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
  (unless (scope? sc)
    (raise-argument-error
     'add-scope
     "scope?"
     sc))
  (internal-definition-context-introduce
    (scope-def-ctx sc)
    ((scope-introducer sc) stx 'add) 'add))

(define (add-scopes stx scs)
  (unless (syntax? stx)
    (raise-argument-error
     'add-scopes
     "syntax?"
     stx))
  (unless (and (list? scs) (andmap scope? scs))
    (raise-argument-error
     'add-scopes
     "(listof scope?)"
     scs))
  
  (for/fold ([stx stx])
            ([sc scs])
    ((scope-introducer sc) stx 'add)))

(define (splice-from-scope stx sc)
  (unless (syntax? stx)
    (raise-argument-error
     'remove-scope
     "syntax?"
     stx))
  (unless (scope? sc)
    (raise-argument-error
     'remove-scope
     "scope?"
     sc))
  (internal-definition-context-introduce
   (scope-def-ctx sc)
   ((scope-introducer sc) stx 'remove)
   'remove))

(define (add-ctx-scope ctx stx)
  (if ctx
    (internal-definition-context-introduce ctx stx 'add)
    stx))

(struct racket-var [])

(define (bind! id rhs-arg)
  (unless (identifier? id)
    (raise-argument-error
     'bind!
     "identifier?"
     id))
  (unless (current-def-ctx)
    (error 'bind!
           "cannot call outside of with-scope"))

  (define rhs
    (if (racket-var? rhs-arg)
        #f
        #`'#,rhs-arg))
  
  (syntax-local-bind-syntaxes (list id) rhs (current-def-ctx))
  (define id-in-sc (add-ctx-scope (current-def-ctx) (syntax-local-identifier-as-binding id)))
  (lift-disappeared-bindings! id-in-sc)
  id-in-sc)

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

  (when (and (not (eq? result unbound)) (predicate result))
    (lift-disappeared-uses! id-in-sc))
  
  result)

(define (syntax-local-introduce-splice stx)
  (syntax-local-identifier-as-binding
   (syntax-local-introduce stx)))

(define (apply-as-transformer f ctx-type-arg . args)
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
  
  (apply prim-apply-as-transformer
         f
         (case ctx-type-arg
           [(expression) 'expression]
           [(definition) (list (current-ctx-id))])
         (if (current-def-ctx) (list (current-def-ctx)) '())
         args))

(begin-for-syntax
  (define-syntax-class ctx-type
    (pattern #:expression
             #:attr type #''expression)
    (pattern #:definition
             #:attr type #''definition)))

(define-syntax define/hygienic
  (syntax-parser
    [(_ (name arg ...) ctx:ctx-type
        body ...+)
     #'(begin
         (define (tmp arg ...)
           body ...)
         (define (name arg ...)
           (apply-as-transformer tmp ctx.type arg ...)))]))

; convenient for cmdline-ee case study
(require syntax/parse/experimental/template)
(provide define/hygienic-metafunction)
(define-syntax define/hygienic-metafunction
  (syntax-parser
    [(_ (name arg) ctx:ctx-type
        body ...)
     #'(begin
         (define (tmp arg)
           body ...)
         (define-template-metafunction (name stx)
           (syntax-parse stx
             [(_ t)
              (apply-as-transformer tmp ctx.type #'t)])))]))

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
