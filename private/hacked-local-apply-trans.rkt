#lang racket/base

(require (for-template racket/base)
         racket/syntax)

(provide local-apply-transformer)

(define ((make-quoting-transformer transformer-proc) stx)
  (syntax-case stx ()
    [(_ form)
     (let ([result (transformer-proc #'form)])
       (unless (syntax? result)
         (raise-arguments-error 'local-apply-transformer
                                "received value from syntax expander was not syntax"
                                "received" result))
       #`(quote #,result))]))

(define (local-apply-transformer transformer stx context [intdef-ctxs '()])
  (unless (or (set!-transformer? transformer)
              (and (procedure? transformer)
                   (procedure-arity-includes? transformer 1)))
    (raise-argument-error 'local-apply-transformer
                          "(or/c (-> syntax? syntax?) set!-transformer?)"
                          transformer))
  (unless (syntax? stx)
    (raise-argument-error 'local-apply-transformer "syntax?" stx))
  (unless (or (eq? context 'expression)
              (eq? context 'top-level)
              (eq? context 'module)
              (eq? context 'module-begin)
              (list? context))
    (raise-argument-error 'local-apply-transformer
                          "(or/c 'expression 'top-level 'module 'module-begin list?)"
                          context))
  (unless (and (list? intdef-ctxs)
               (andmap internal-definition-context? intdef-ctxs))
    (raise-argument-error 'local-apply-transformer
                          "(listof internal-definition-context?)"
                          intdef-ctxs))
  (unless (syntax-transforming?)
    (raise-arguments-error 'local-apply-transformer "not currently expanding"))
  (let* ([intdef-ctx (syntax-local-make-definition-context #f #f)]
         [transformer-proc (if (set!-transformer? transformer)
                               (set!-transformer-procedure transformer)
                               transformer)]
         [transformer-id (internal-definition-context-introduce
                          intdef-ctx
                          (generate-temporary 'local-apply-transformer))]
         [neutral-transformer-id (syntax-local-introduce transformer-id)]
         [exprhack-id (internal-definition-context-introduce
                       intdef-ctx
                       (generate-temporary 'local-apply-transformer-exprhack))]
         [intdef-ctxs* (cons intdef-ctx intdef-ctxs)])
    (syntax-local-bind-syntaxes
     (list transformer-id)
     #`(quote #,(make-quoting-transformer transformer-proc))
     intdef-ctx)
    (syntax-local-bind-syntaxes
     (list exprhack-id)
     #`(quote #,(lambda (stx)
                  (syntax-case stx ()
                    [(_ arg)
                     ; Continued workaround for https://github.com/racket/racket/pull/2237:
                     ; Force an expression context; with Racket's current bug this will set the
                     ; use-site field to #f. That will then trigger nested definition context
                     ; expansions to make a new def-ctx. Would not work for traditional interpreter
                     ; -like use of definition contexts, but does work when the bind! happens
                     ; inside the local expansion.
                     (local-expand #`(#,(syntax-local-introduce neutral-transformer-id) #,#'arg)
                                   'expression
                                   '()
                                   intdef-ctxs*)])))
     intdef-ctx)
    
    (define expanded
      (if (eq? context 'expression)
          ; Expand as a definition first to get a use-site scope, as a workaround for
          ; https://github.com/racket/racket/pull/2237
          (local-expand #`(#,exprhack-id #,stx) (list (gensym)) '() intdef-ctxs*)
          (local-expand #`(#,transformer-id #,stx) context '() intdef-ctxs*)))

    (syntax-case expanded (quote)
      [(quote form) #'form])))