#lang racket/base

(require (for-syntax racket/base))

(provide
 (for-syntax
  lift-disappeared-uses!
  lift-disappeared-bindings!))

(begin-for-syntax
  (define lifted? #f)
  (define uses '())
  (define bindings '())

  (define (ensure-module-end!)
    (when (not lifted?)
      (syntax-local-lift-module-end-declaration #'(emit-lifteds)))
    (set! lifted? #t))

  (define (adjust-id-for-lift id)
    ; syntax-local-introduce may flip back on use-site scopes, so take them back off.
    (syntax-local-identifier-as-binding
     (syntax-local-introduce
      id)))

  (define (lift-disappeared-uses! . ids)
    (ensure-module-end!)
    (set! uses (append (map adjust-id-for-lift ids) uses)))

  (define (lift-disappeared-bindings! . ids)
    (ensure-module-end!)
    (set! bindings (append (map adjust-id-for-lift ids) bindings))))

(define-syntax (emit-lifteds stx)
  (define result
    (syntax-property
     (syntax-property
      #'(void)
      'disappeared-use
      uses)
     'disappeared-binding
     bindings))

  ; Reset so another end declaration lift is triggered if macros
  ; that are themselves end declarations lift disappeareds.
  (set! lifted? #f)
  (set! uses '())
  (set! bindings '())

  result)