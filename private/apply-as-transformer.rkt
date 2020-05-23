#lang racket/base

(require "hacked-local-apply-trans.rkt")

(provide
 ; Version of apply-as-transformer with a `local-expand`-like API.
 apply-as-transformer)

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

(define (apply-as-transformer f ctx-type ctxs . args)
  (define (single-argument-transformer stx)
    (call-with-values
     (lambda () (apply f (map unwrap (syntax->list stx))))
     (lambda vs (datum->syntax #f (map wrap vs)))))

  (define res
    (local-apply-transformer
     single-argument-transformer
     (datum->syntax #f (map wrap args))
     ctx-type
     ctxs))

  (apply values (map unwrap (syntax->list res))))

