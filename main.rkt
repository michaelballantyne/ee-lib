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
  (for-template racket/base)

  "private/flip-intro-scope.rkt")

(provide
 flip-intro-scope
 same-binding?

 qstx/rc ; read as quasisyntax/loc+props
 qstx/lp

 bind!
 racket-var
 racket-var?
 with-scope
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
 definition-macro)

(define (bound? id)
  (identifier-binding id (syntax-local-phase-level) #t))

(define (same-binding? id1 id2)
  (let ([id1-ext (if (syntax-transforming?) (flip-intro-scope id1) id1)]
        [id2-ext (if (syntax-transforming?) (flip-intro-scope id2) id2)])
    (and (bound? id1-ext)
         (bound? id2-ext)
         (free-identifier=? id1-ext id2-ext))))

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

(define (call-with-scope p)
  (let* ([ctx (syntax-local-make-definition-context (current-def-ctx))])
    (parameterize ([current-def-ctx ctx]
                   [current-ctx-id (gensym 'with-scope-ctx)])
      (p ctx))))

(define-simple-macro
  (with-scope name:id body ...)
  (call-with-scope (lambda (name) body ...)))

(define (add-scope stx sc)
  (unless (syntax? stx)
    (raise-argument-error
     'add-scope
     "syntax?"
     stx))
  (unless (internal-definition-context? sc)
    (raise-argument-error
     'add-scope
     "internal-definition-context?"
     sc))
  (internal-definition-context-add-scopes sc stx))

(define (add-scopes stx scs)
  (unless (syntax? stx)
    (raise-argument-error
     'add-scopes
     "syntax?"
     stx))
  (unless (and (list? scs) (andmap internal-definition-context? scs))
    (raise-argument-error
     'add-scopes
     "(listof internal-definition-context?)"
     scs))
  
  (for/fold ([stx stx])
            ([sc scs])
    (internal-definition-context-add-scopes sc stx)))

(define (splice-from-scope id sc)
  (unless (identifier? id)
    (raise-argument-error
     'splice-from-scope
     "identifier?"
     id))
  (unless (internal-definition-context? sc)
    (raise-argument-error
     'splice-from-scope
     "internal-definition-context?"
     sc))
  (internal-definition-context-splice-binding-identifier sc id))

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

  (define ids-in-sc
    (syntax-local-bind-syntaxes
     (if (list? id) id (list id))
     rhs
     (current-def-ctx)))

  (define ids-with-prop
    (for/list ([id ids-in-sc])
      (syntax-property id 'binder #t)))

  (apply lift-disappeared-bindings! ids-with-prop)
  (if (list? id) ids-with-prop (car ids-with-prop)))

(define (eval-transformer stx)
  (syntax-local-eval stx (or (current-def-ctx) '())))

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
   (syntax-local-introduce stx)
   (current-def-ctx)))

(define (apply-as-transformer f f-id ctx-type-arg . args)
  (unless (procedure? f)
    (raise-argument-error
     'apply-as-transformer
     "procedure?"
     f))

  (unless (or (identifier? f-id)
              (not f-id))
    (raise-argument-error
     'apply-as-transformer
     "(or/c identifier? #f)"
     f-id))
  
  (unless (member ctx-type-arg '(expression definition))
    (raise-argument-error
     'apply-as-transformer
     "(or/c 'expression 'definition)"
     ctx-type-arg))

  (apply-with-hygiene f f-id ctx-type-arg #t args))

(define (syntax-local-apply-transformer-use-site-workaround
         f f-id ctx-type def-ctx . args)
  (define (maybe-flip v)
    (if (syntax? v) (flip-intro-scope v) v))
  (if (eq? ctx-type 'expression)
      ; Expand as a definition first to get a use-site scope, as a workaround for
      ; https://github.com/racket/racket/pull/2237
      (let ([f-id^ (maybe-flip f-id)])
        (apply syntax-local-apply-transformer
               (lambda args
                 (apply syntax-local-apply-transformer f (maybe-flip f-id^) 'expression def-ctx args))
               f-id (list (gensym)) def-ctx args))
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
           ; Hack: Provide a name from racket/base (which we require for-template)
           ; as binding-id to avoid creation of use-site scopes for define/hygienic.
           ;
           ; We don't need use-site scopes here because we know that *all*
           ; invocations of define/hygienic generate syntax with unique scopes,
           ; so syntax from a use (that is, one invocation) can't bind syntax
           ; from another invocation.
           ;
           ; Interface macros also generate syntax with unique scopes, so we don't
           ; have to worry about use-site binders from those entry points either.
           (apply-with-hygiene tmp #'car ctx.type ctx.seal? (list arg ...))))]))

; Convenient for cmdline-ee case study
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
              ; Hack: See discussion of binding-id in define/hygienic.
              (apply-as-transformer tmp #'car ctx.type #'t)])))]))

; Applies the function f to each element of the tree, starting
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
;   May only be used in test/suspend/suspend.rkt now.
; Note that uses of free-id-table-ref to access these entries also need
; flip-intro-scope; perhaps I need a helper for that too.
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
                      (flip-intro-scope id)
                      result)
  
  (flip-intro-scope
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
