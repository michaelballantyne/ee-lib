#lang racket/base

(require
  syntax/apply-transformer
  racket/syntax
  syntax/parse
  racket/class
  syntax/id-table
  (for-template "lift-disappeared.rkt")
  (for-syntax
   racket/base
   syntax/parse
   racket/syntax
   syntax/transformer
   (only-in syntax/parse [define/syntax-parse def/stx]))
  (for-template racket/base))

(provide
 qstx/rc ; read as quasisyntax/loc+props
 qstx/lp

 bind!
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
 current-def-ctx
 current-ctx-id
 current-local-def-ctxs

 eval-transformer

 map-transform
 syntax-local-introduce-splice
 fresh-symbol-table%)

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


; Light wrappers around the scope and definition context APIs for convenience
; and automatic disappeared tracking. It would be really nice to make something
; like these part of the standard library...

(struct scope [introducer def-ctx])

(define (make-scope)
  (let ([ctx (syntax-local-make-definition-context (current-def-ctx))])
    (values (scope (make-syntax-introducer #t) ctx)
            ctx)))

(define-syntax-rule
  (with-scope name body ...)
  (let-values ([(name ctx) (make-scope)])
    (parameterize ([current-def-ctx ctx]
                   [current-local-def-ctxs (cons ctx (current-local-def-ctxs))]
                   [current-ctx-id (gensym 'with-scope-ctx)])
      (let ()
        body ...))))

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
  ((scope-introducer sc) stx 'add))

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


(define (make-def-ctx) (syntax-local-make-definition-context))

(define (add-ctx-scope ctx stx)
  (if ctx
      (internal-definition-context-introduce ctx stx 'add)
      stx))

(define (add-ctxs-scopes ctxs stx)
  (for/fold ([stx stx])
            ([ctx ctxs])
    (add-ctx-scope ctx stx)))

(define (bind! id rhs)
  (define ctx (current-def-ctx))
  (unless (internal-definition-context? ctx)
    (raise-argument-error
     'bind!
     "internal-definition-context?"
     ctx))
  (unless (identifier? id)
    (raise-argument-error
     'bind!
     "identifier?"
     id))
  #;(unless (or (not rhs) (syntax? rhs))
      (raise-argument-error
       'bind!
       "(or/c #f syntax?)"
       rhs))

  ; TODO: fix bug. should put all current-local-def-ctxs on RHS as well.
  (syntax-local-bind-syntaxes (list id) #`'#,rhs ctx)
  (define id-in-sc (add-ctxs-scopes (current-local-def-ctxs) (syntax-local-identifier-as-binding id)))
  (lift-disappeared-bindings! id-in-sc)
  id-in-sc)

; used only for eq? equality.
(define unbound
  (let ()
    (struct unbound [])
    (unbound)))

(define (lookup id)
  (define ctx (current-def-ctx))
  (unless (or (not ctx) (internal-definition-context? ctx))
    (raise-argument-error
     'lookup
     "(or/c #f internal-definition-context?)"
     ctx))
  (unless (identifier? id)
    (raise-argument-error
     'lookup
     "identifier?"
     id))
  
  (define id-in-sc (add-ctxs-scopes (current-local-def-ctxs) id))
  (define result
    (syntax-local-value
     id-in-sc
     (lambda () unbound)
     ctx))

  (unless (eq? result unbound)
    (lift-disappeared-uses! id-in-sc))
  
  result)


(define (syntax-local-introduce-splice stx)
  (syntax-local-identifier-as-binding
   (syntax-local-introduce stx)))

; Apply as transformer. Perhaps should eventually be added to
; syntax/apply-transformer?

(struct wrapper (contents))

(define (wrap arg)
  (if (syntax? arg)
      arg
      (wrapper arg)))

(define (unwrap arg)
  (if (syntax? arg)
      (let ([e (syntax-e arg)])
        (if (wrapper? e)
            (wrapper-contents e)
            arg))
      arg))

(define current-def-ctx (make-parameter #f))
(define current-ctx-id (make-parameter #f))
(define current-local-def-ctxs (make-parameter '()))

(define (apply-as-transformer f ctx-type-arg . args)
  (define before (car args))
  (unless (procedure? f)
    (raise-argument-error
     'apply-as-transformer
     "procedure?"
     f))

  (define (single-argument-transformer stx-arg)
    (define stx (add-ctxs-scopes (current-local-def-ctxs) stx-arg))
    (define (go)
      (call-with-values
       (lambda () (apply f (map unwrap (syntax->list stx))))
       (lambda vs (datum->syntax #f (map wrap vs)))))

    (case ctx-type-arg
      [(expression)
       (parameterize ([current-local-def-ctxs '()])
         (go))]
      [(definition)
       (go)]))

  (define ctx-type
    (case ctx-type-arg
      [(expression) 'expression]
      [(definition)
       (let ([ctx-id (current-ctx-id)])
         #;(unless ctx-id
             (error 'apply-as-transformer "cannot call definition-context expander outside of define/hygienic"))
         (if ctx-id (list ctx-id) (syntax-local-context)))]))
  
  (define ctx (current-def-ctx))
  (define res
    (local-apply-transformer
     single-argument-transformer
     (datum->syntax #f (map wrap args))
     ctx-type
     (cond
       [(internal-definition-context? ctx) (list ctx)]
       [(list? ctx) ctx]
       [(not ctx) '()]
       [else (raise-argument-error
              'apply-as-transformer
              "(or/c internal-definition-context? (listof internal-definition-context?) #f)"
              ctx)])))
  (define after (car (map unwrap (syntax->list res))))
  (apply values (map unwrap (syntax->list res))))

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

(define-syntax generic+rep
  (syntax-parser
    #:literals (define)
    [(_ name
        (fields)
        (define header . body) ...)
     #'(begin
         (define-generics name))]))

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

(define fresh-symbol-table%
  (class object%
    (super-new)
    (field [table (make-free-id-table)])

    (define/public (compile-name! id)
      (let ([result (generate-temporary id)])
        (free-id-table-set! table (syntax-local-introduce id) result)
        (syntax-local-introduce
         result)))

    (define/public (compiled-name id)
      (syntax-local-introduce
       (free-id-table-ref table (syntax-local-introduce id))))))

(define (eval-transformer stx)
  (define ctx (syntax-local-make-definition-context (current-def-ctx)))
  (define id (generate-temporary #'x))
  (syntax-local-bind-syntaxes (list id) (add-ctxs-scopes (current-local-def-ctxs) stx) ctx)
  (syntax-local-value (internal-definition-context-introduce ctx id 'add) (lambda () (error 'eval-transformer "shouldn't happen")) ctx))