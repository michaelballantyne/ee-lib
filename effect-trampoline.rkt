#lang racket/base

(provide
 effect-trampoline-macro
 trampoline-lift!
 current-persistent-intro-scope)

(require
  syntax/parse
  racket/match
  racket/control
  (for-template racket/base))

; Provided fns

(define current-persistent-intro-scope (make-parameter #f))

(define (effect-trampoline-macro initial-transformer)
  (define persistent-intro-scope (make-syntax-introducer))

  (lambda (stx)
    (define adjusted-stx-arg (args->neg stx persistent-intro-scope))
  
    (call-with-module-effects
     persistent-intro-scope
     (lambda ()
       (initial-transformer adjusted-stx-arg)))))

(define (trampoline-lift! stx)
  (call/cc
   (lambda (cc)
     (abort/cc syntax-effect-prompt stx cc))
   syntax-effect-prompt))

; Helpers for effect control

(define syntax-effect-prompt (make-continuation-prompt-tag))

(struct syntax-effect [stx k])

(define (call-with-module-effects persistent-intro-scope transformer-k)
  (parameterize ([current-persistent-intro-scope persistent-intro-scope])
    (match (call-with-continuation-prompt
            transformer-k
            syntax-effect-prompt
            syntax-effect)
      [(syntax-effect stx k)
       (construct-effect-syntax (neg->return stx persistent-intro-scope)
                                (lambda ()
                                  (call-with-module-effects
                                   persistent-intro-scope
                                   k)))]
      [stx (neg->return stx persistent-intro-scope)])))

; Helpers for trampolining

(define (construct-effect-syntax syntax-to-emit transformer-k)
  #`(begin
      #,syntax-to-emit
      (apply-k #,transformer-k)))

(module apply-k racket/base
  (require (for-syntax racket/base))
  (provide apply-k)
  (define-syntax (apply-k stx)
    (syntax-case stx ()
      [(_ k-proc) ((syntax->datum #'k-proc))])))
(require (for-template 'apply-k))

; Helpers for scope flipping

(define (args->neg stx persistent-intro-scope)
  (persistent-intro-scope ((current-intro-scope) stx 'remove) 'add))

(define (neg->return stx persistent-intro-scope)
  ((current-intro-scope)
   (persistent-intro-scope stx 'flip)
   'add))

(define (pos<->neg stx persistent-intro-scope)
  (persistent-intro-scope stx 'flip))

(define (current-intro-scope)
  (define no-scopes-stx (datum->syntax #f 'an-id))
  
  (define ctx (syntax-local-make-definition-context))
  (syntax-local-bind-syntaxes (list #'x) #`(quote-syntax #,no-scopes-stx) ctx)
  (define intro+defctx-scopes-stx (syntax-local-value (internal-definition-context-introduce ctx #'x 'add)
                                                      #f ctx))
  
  (define intro-scope-stx (internal-definition-context-introduce ctx intro+defctx-scopes-stx 'remove))
  
  (make-syntax-delta-introducer no-scopes-stx intro-scope-stx))
