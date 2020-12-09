#lang racket

; Just a small file to play with without having the definition of the language in scope

(require "js.rkt" (for-syntax syntax/parse))

(module+ test
  (when (not (getenv "PLT_PKG_BUILD_SERVICE"))
(js ((function ()
                 (let x 5)
                 (let-syntax m (lambda (stx)
                                 (syntax-parse stx
                                   [(_ arg)
                                    #'((function (arg) (return x)) 6)])))
                 x
                 (return (m x)))))))
