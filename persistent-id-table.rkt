#lang racket/base

(require
  racket/base
  racket/set
  racket/prefab
  racket/private/check
  syntax/id-table
  (for-template racket/base)
  (for-syntax racket/base syntax/parse)
  "private/flip-intro-scope.rkt")

(provide
 simple-datum?
 define-persistent-free-id-table
 persistent-free-id-table?
 persistent-free-id-table-set!
 persistent-free-id-table-ref
 persist-free-id-table-extensions!
 wrap-persist)

(define (simple-datum? v)
  (or (null? v)
      (symbol? v)
      (boolean? v)
      (number? v)
      (and (pair? v) (simple-datum? (car v)) (simple-datum? (cdr v)))
      (and (vector? v) (for/and ([el v]) (simple-datum? el)))
      (and (box? v) (simple-datum? (unbox v)))
      (and (hash? v) (for/and ([(k v) v]) (and (simple-datum? k) (simple-datum? v))))
      (and (immutable-prefab-struct-key v) (for/and ([el (in-vector (struct->vector v) 1)])
                                             (simple-datum? el)))))

; Design note: we can't persist via a lift because that'd end up at the end of the module,
; so entries wouldn't be available during module visit until the end of the module
; is reached.

(struct persistent-free-id-table [persisted transient id])

(define (make-persistent-free-id-table id)
  (persistent-free-id-table
   (make-free-id-table)
   (make-free-id-table)
   id))

(define tables-needing-persist (mutable-seteq))

(define/who (persistent-free-id-table-set! t id val)
  (check who persistent-free-id-table? t)
  (check who identifier? id)
  (check who (lambda (v) (or (syntax? v) (simple-datum? v)))
         #:contract "(or/c syntax? simple-datum?)"
         val)
  
  (set-add! tables-needing-persist t)
  (free-id-table-set! (persistent-free-id-table-transient t) id val))

(define (ref-error)
  (error 'persistent-free-id-table-ref "no value found for key"))

(define/who (persistent-free-id-table-ref t id [fail ref-error])
  (check who persistent-free-id-table? t)
  (check who identifier? id)

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

(define/who (persist-free-id-table-extensions! t)
  (check who persistent-free-id-table? t)
  
  (define alist
    (for/list ([(k v) (in-free-id-table (persistent-free-id-table-transient t))])
      #`(cons #'#,(flip-intro-scope k) #,(if (syntax? v)
                                             #`#'#,(flip-intro-scope v)
                                             #`'#,v))))
  #`(begin-for-syntax
      (do-extension! #,(persistent-free-id-table-id t)
                     (list . #,alist))))

(define (persist-all-free-id-table-extensions!)
  (define result
    #`(begin
        #,@(for/list ([t tables-needing-persist])
             (persist-free-id-table-extensions! t))))
  (set-clear! tables-needing-persist)
  result)

(define/who (wrap-persist transformer)
  (check who procedure? transformer)
  
  (lambda (stx)
    (check 'wrap-persist-transformer syntax? stx)
    
    (unless (eq? 'module (syntax-local-context))
      (error 'wrap-persist-transformer "wrap-persist transformer may only be used in module context"))
    
    (define t-result (transformer stx))
    (check 'wrap-persist-transformer syntax? t-result)
    #`(begin
        #,t-result
        #,(persist-all-free-id-table-extensions!))))

(define-syntax define-persistent-free-id-table
  (syntax-parser
    [(_ name:id)
     (unless (eq? 'module (syntax-local-context))
       (raise-syntax-error #f "only allowed in module context" this-syntax))
     #'(define name (make-persistent-free-id-table (quote-syntax name)))]))

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
    #`#'#,(persistent-free-id-table-ref v #'x))

  (check-equal?
   (syntax->datum (m2))
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
