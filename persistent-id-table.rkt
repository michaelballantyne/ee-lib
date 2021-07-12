#lang racket/base

(require
  racket/base
  syntax/id-table
  (for-template racket/base)
  "private/flip-intro-scope.rkt")

(provide
 define-persistent-free-id-table
 persistent-free-id-table?
 persistent-free-id-table-set!
 persistent-free-id-table-ref
 persist-free-id-table-extensions!)

; Design note: we can't persist via a lift because that'd end up at the end of the module,
; so entries wouldn't be available during module visit until ithe end of the module
; is reached.

(struct persistent-free-id-table [persisted transient id])

(define (make-persistent-free-id-table id)
  (persistent-free-id-table
   (make-free-id-table)
   (make-free-id-table)
   id))

(define (persistent-free-id-table-set! t id val)
  (free-id-table-set! (persistent-free-id-table-transient t) id val))

(define (ref-error)
  (error 'persistent-free-id-table-ref "no value found for key"))

(define (persistent-free-id-table-ref t id [fail ref-error])
  (define (try-persistent)
    (free-id-table-ref
     (persistent-free-id-table-persisted t)
     id
     fail))
  (free-id-table-ref
   (persistent-free-id-table-transient t) id
   try-persistent))

(define (do-extension! t alist)
  (define p (persistent-free-id-table-persisted t))
  (for ([pair alist])
    (free-id-table-set! p (car pair) (cdr pair))))

(define (persist-free-id-table-extensions! t)
  (define alist
    (for/list ([(k v) (in-free-id-table (persistent-free-id-table-transient t))])
      #`(cons #'#,(flip-intro-scope k) '#,v)))
  #`(begin-for-syntax
      (do-extension! #,(persistent-free-id-table-id t)
                     (list . #,alist))))

(define-syntax-rule (define-persistent-free-id-table v)
  (define v (make-persistent-free-id-table #'v)))

(module* test racket/base
  (require
    rackunit
    (for-syntax
     racket/base
     rackunit
     (submod "..")))

  (begin-for-syntax
    (define-persistent-free-id-table v))

  (define-syntax (m1 stx)
    (persistent-free-id-table-set! v #'x 5)
    #'(void))
  (m1)

  (define-syntax (m2 stx)
    #`#,(persistent-free-id-table-ref v #'x))

  (check-equal?
   (m2)
   5)
  
  (define-syntax (m3 stx)
    (persist-free-id-table-extensions! v))

  (m3)

  (define-syntax (m5 stx)
    #`#,(persistent-free-id-table-ref v #'y #f))

  (check-equal?
   (m5)
   #f)
  
  (begin-for-syntax
    (check-equal?
     (persistent-free-id-table-ref v #'x)
     5))
  )
