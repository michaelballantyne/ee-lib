#lang racket/base

(provide identifier-has-binding?
         identifier-with-binding?
         same-binding?
         module-or-top-binding?
         )

(require racket/private/check
         "flip-intro-scope.rkt")

(define/who (identifier-has-binding? id)
  (check who identifier? id)
  
  (not (not (identifier-binding id (syntax-local-phase-level) #t))))

(define (identifier-with-binding? val)
  (and (identifier? val) (identifier-has-binding? val)))

(define/who (same-binding? id1 id2)
  (check who identifier? id1)
  (check who identifier? id2)
  
  (let ([id1-ext (if (syntax-transforming?) (flip-intro-scope id1) id1)]
        [id2-ext (if (syntax-transforming?) (flip-intro-scope id2) id2)])
    (and (identifier-has-binding? id1-ext)
         (identifier-has-binding? id2-ext)
         (free-identifier=? id1-ext id2-ext))))

(define/who (module-or-top-binding? id)
  (check who identifier-with-binding? id)

  (define binding
    (identifier-binding id (syntax-local-phase-level) #t))
  (list? binding))