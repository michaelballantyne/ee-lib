#lang racket/base

(require (for-syntax racket/base "flip-intro-scope.rkt"))

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
      (syntax-local-lift-expression #'(emit-lifteds)))
    (set! lifted? #t))

  (define (lift-disappeared-uses! . ids)
    (ensure-module-end!)
    (set! uses (append (map flip-intro-scope ids) uses)))

  (define (lift-disappeared-bindings! . ids)
    (ensure-module-end!)
    (set! bindings (append (map flip-intro-scope ids) bindings))))

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
