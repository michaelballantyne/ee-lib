#lang racket/base

(provide serialize-syntax deserialize-syntax)

(require racket/match)

;; Notes and ideas

;; How do we avoid collisions between the encoding and normal syntax object content now?
;;   - gensyms we generate for our table will be different than any in the input, so if we look it up and find it
;;     then we know it has our meaning.
;;   - only such gensyms occur directly within syntax or datum content; the structures do not.

;; The expander will retain graph structure for us I think. Where nodes in the shared graph are syntax objects.
;; Assuming we use dedup properly such that when we encounter the same syntax object twice we don't recur inside twice.
;;
;; So it seems like we are properly preserving sharing right now, as best as I can tell. But we're wasting work deeply
;; walking the full tree structure rather than cutting off fast and re-using the work we've already done. So that's a perf
;; problem to fix.
;;
;; Options on improving the tree walking:
;; 1. Use datum-map and non-syntax-map, with dedup everywhere.
;;     Should preserve all sharing. Would avoid repetitive work on shared objects in syntax given that datum->syntax
;;     unfolds shared structure outside of syntax wrappers. Would not avoid repeated work in shared datums in properties.
;; 2. Revise datum-map with a `seen` table that it will check on entrance and use the recorded
;;     result if present. This would preserve `eq` sharing throughout.

;; Alternate encoding idea: only replace syntax values inside properties with gensyms.
;; If we use syntax properties on syntax object but just lift out syntax parts within them, we could:
;; 1. Avoid extra structures for stx-with-props, syntax-val, and datum-val
;; 2. Avoid hash table entries for syntax with properties, only needing them for syntax *in* properties
;; 3. Avoid wraping datums in properties as syntax
;; 4. Have perhaps a more readable output, though not showing the properties



;; table :: HashOf gensym stx-with-props
;; contents :: syntax?
(struct serialized-syntax (table contents) #:prefab)

;; ps :: AList symbol (or syntax-val datum-val)
(struct stx-with-props (stx ps) #:prefab)
(struct syntax-val (stx) #:prefab)
(struct datum-val (d) #:prefab)

;(require racket/pretty)

(define (has-props? stx)
  (for/or ([p (syntax-property-symbol-keys stx)])
    (syntax-property-preserved? stx p)))

(define (serialize-syntax stx)
  (define gensym
    (let ([ctr 0])
      (lambda ()
        (set! ctr (+ ctr 1))
        (string->uninterned-symbol (string-append "stx-" (number->string ctr))))))
  
  (define table (hasheq))
  (define dedup-table (hasheq))
  (define (dedup k f)
    (if (hash-has-key? dedup-table k)
      (hash-ref dedup-table k)
      (let ([res (f)])
        (set! dedup-table (hash-set dedup-table k res))
        res)))

  (define (lift! el)
    (define tag-sym (gensym))
    (set! table (hash-set table tag-sym el))
    tag-sym)

  (define (build-props! orig-s d)
    (stx-with-props
     (datum->syntax orig-s d orig-s #f)
     (for/list ([k (syntax-property-symbol-keys orig-s)]
                #:when (syntax-property-preserved? orig-s k))
       (define val (syntax-property orig-s k))
       (define serialized-val
         (if (syntax? val)
             (syntax-val (serialize-element! val))
             (datum-val (serialize-element! val #:always-lift? #t))))
       (cons k serialized-val))))

  ;; When #:always-lift? is true, any syntax element within will be lifted
  ;; into the hash, even if it does not have properties. This is used to ensure
  ;; syntax within datum-val gets lifted to establish the boundary between datum and syntax.
  (define (serialize-element! el #:always-lift? [always-lift? #f])
    (dedup
      el
      (lambda ()
        (syntax-map
          el
          ;; at each datum element
          (lambda (tail? d) d)
          ;; at each syntax wrapper
          (lambda (orig-s d)
            ;(when (and always-lift? (not (ref? (hash-ref dedup-table orig-s)))) ; TODO
            ;(error 'dedup "lift error"))
            (dedup
              orig-s
              (lambda ()
                (if (or always-lift? (has-props? orig-s))
                  (lift! (build-props! orig-s d))
                  (datum->syntax orig-s d orig-s #f)))))
          syntax-e))))

  (define top-s (serialize-element! stx))
  (define res (datum->syntax #f (serialized-syntax table top-s)))

  res)

(define (deserialize-syntax ser)
  (match (syntax-e ser)
    [(serialized-syntax table-stx contents)
     (define table (syntax-e table-stx))
     (define dedup-table (hasheq))
     (define (dedup k f)
       (if (hash-has-key? dedup-table k)
         (hash-ref dedup-table k)
         (let ([res (f)])
           (set! dedup-table (hash-set dedup-table k res))
           res)))


     (define (maybe-syntax-e v)
       (if (syntax? v) (syntax-e v) v))

     (define (deserialize-stx-with-props ref-sym)
       (match-define (stx-with-props stx ps) (syntax-e (hash-ref table ref-sym)))
       (define deserialized-nested-stx (deserialize-element stx))
       (for/fold ([stx deserialized-nested-stx])
                 ([stx-pr (syntax->list ps)])
         (define pr (syntax-e stx-pr))
         (define k (syntax-e (car pr)))
         (define v (syntax-e (cdr pr)))
         (define prop-val
           (match v
             [(syntax-val v)
              (deserialize-element v)]
             [(datum-val v)
              (deserialize-element (syntax->datum v))]))
         (syntax-property stx k prop-val #t)))

     (define (deserialize-element el)
       (dedup
         el
         (lambda ()
           (syntax-map
             el
             (lambda (tail? d)
               (if (and (symbol? d) (hash-has-key? table d))
                   (dedup
                    d
                    (lambda () (deserialize-stx-with-props d)))
                   d))
             (lambda (orig-s d)
               (dedup
                 orig-s
                 (lambda () (datum->syntax orig-s d orig-s #f))))
             syntax-e))))

     (define res (deserialize-element contents))
     res]))
   
(module+ test
  (require rackunit)

  (define type
    (syntax-property
      (syntax-property #'Int ':: #'Type #t)
      'orig (list #'Int) #t))
  (define term (syntax-property #`(1 #,(syntax-property #'2 ': type #t)) ': #'Type #t))
  (define s (serialize-syntax term))
  (define d (deserialize-syntax s))

  (check-true
   (bound-identifier=?
    (syntax-property d ':)
    #'Type))

  ; syntax with properties inside outer syntax with properties.
  (check-true
   (bound-identifier=?
    (syntax-property (syntax-property (cadr (syntax-e d)) ':) '::)
    #'Type))

  (check-true
   (bound-identifier=?
    (syntax-property (cadr (syntax-e d)) ':)
    #'Int))

  (check-equal?
   (syntax-position term)
   (syntax-position d))
  
  (check-equal?
   (syntax-position (syntax-property (cadr (syntax-e term)) ':))
   (syntax-position (syntax-property (cadr (syntax-e d)) ':)))

  (check-equal?
   (syntax-position (car (syntax-e term)))
   (syntax-position (car (syntax-e d))))

  ; syntax in datum in properties
  (check-true
    (bound-identifier=?
      (car (syntax-property (syntax-property (cadr (syntax-e d)) ':) 'orig))
      #'Int))

  (let ()
    (define s1 #'Foo)
    (define s2 #`(#,s1 #,s1))
    (syntax-case (deserialize-syntax (serialize-syntax s2)) ()
      [(a b)
       (check-eq? #'a #'b)])
    
    (define s3 (syntax-property #`(#,s1) 'foo s1 #t))
    (define s3-d (deserialize-syntax (serialize-syntax s3)))
    (check-eq? (syntax-case s3-d () [(a) #'a]) (syntax-property s3-d 'foo))

    (define v1 (cons 'x 'x))
    (define s4 (syntax-property #'y 'foo v1 #t))
    (define v1-d (syntax-property (deserialize-syntax (serialize-syntax s4)) 'foo))
    (check-eq? (car v1-d) (cdr v1-d))
    )
  )


;; ----------------------------------------------------------------

;; syntax-map and datum-map copied from the expander files
;;    syntax/datum-map.rkt
;;    syntax/syntax.rkt

(require racket/fixnum racket/prefab)

;; `(datum-map v f)` walks over `v`, traversing objects that
;; `datum->syntax` traverses to convert content to syntax objects.
;; 
;; `(f tail? d)` is called on each datum `d`, where `tail?`
;; indicates that the value is a pair/null in a `cdr` --- so that it
;; doesn't need to be wrapped for `datum->syntax`, for example;
;; the `tail?` argument is actually #f or a fixnum for a lower bound
;; on `cdr`s that have been taken
;;
;; `gf` is like `f`, but `gf` is used when the argument might be
;; syntax; if `gf` is provided, `f` can assume that its argument
;; is not syntax
;;
;; If a `seen` argument is provided, then it should be an `eq?`-based
;; hash table, and cycle checking is enabled; when a cycle is
;; discovered, the procedure attached to 'cycle-fail in the initial
;; table is called
;;
;; If a `known-pairs` argument is provided, then it should be an
;; `eq?`-based hash table to map pairs that can be returned as-is
;; in a `tail?` position

;; The inline version uses `f` only in an application position to
;; help avoid allocating a closure. It also covers only the most common
;; cases, defering to the general (not inlined) function for other cases.
(define (datum-map s f [gf f] [seen #f] [known-pairs #f])
  (let loop ([tail? #f] [s s] [prev-depth 0])
    (define depth (fx+ 1 prev-depth)) ; avoid cycle-checking overhead for shallow cases
    (cond
      [(and seen (depth . fx> . 32))
       (datum-map-slow tail? s  (lambda (tail? s) (gf tail? s)) seen known-pairs)]
      [(null? s) (f tail? s)]
      [(pair? s)
       (f tail? (cons (loop #f (car s) depth)
                      (loop 1 (cdr s) depth)))]
      [(symbol? s) (f #f s)]
      [(boolean? s) (f #f s)]
      [(number? s) (f #f s)]
      [(or (vector? s) (box? s) (prefab-struct-key s) (hash? s))
       (datum-map-slow tail? s (lambda (tail? s) (gf tail? s)) seen known-pairs)]
      [else (gf #f s)])))

(define (datum-map-slow tail? s f seen known-pairs)
  (let loop ([tail? tail?] [s s] [prev-seen seen])
    (define seen
      (cond
        [(and prev-seen (datum-has-elements? s))
         (cond
           [(hash-ref prev-seen s #f)
            ((hash-ref prev-seen 'cycle-fail) s)]
           [else (hash-set prev-seen s #t)])]
        [else prev-seen]))
    (cond
      [(null? s) (f tail? s)]
      [(pair? s)
       (cond
         [(and known-pairs
               tail?
               (hash-ref known-pairs s #f))
          s]
         [else
          (f tail? (cons (loop #f (car s) seen)
                         (loop (if tail? (fx+ 1 tail?) 1)  (cdr s) seen)))])]
      [(or (symbol? s) (boolean? s) (number? s))
       (f #f s)]
      [(vector? s)
       (f #f (vector->immutable-vector
              (for/vector #:length (vector-length s) ([e (in-vector s)])
                (loop #f e seen))))]
      [(box? s)
       (f #f (box-immutable (loop #f (unbox s) seen)))]
      [(immutable-prefab-struct-key s)
       => (lambda (key)
            (f #f
               (apply make-prefab-struct
                      key
                      (for/list ([e (in-vector (struct->vector s) 1)])
                        (loop #f e seen)))))]
      [(and (hash? s) (immutable? s))
       (cond
         [(hash-eq? s)
          (f #f
             (for/hasheq ([(k v) (in-hash s)])
               (values k (loop #f v seen))))]
         [(hash-eqv? s)
          (f #f
             (for/hasheqv ([(k v) (in-hash s)])
               (values k (loop #f v seen))))]
         [else
          (f #f
             (for/hash ([(k v) (in-hash s)])
               (values k (loop #f v seen))))])]
      [else (f #f s)])))

(define (datum-has-elements? d)
  (or (pair? d)
      (vector? d)
      (box? d)
      (immutable-prefab-struct-key d)
      (and (hash? d) (immutable? d) (positive? (hash-count d)))))

;; `(syntax-map s f d->s)` walks over `s`:
;; 
;;  * `(f tail? d)` is called to each datum `d`, where `tail?`
;;    indicates that the value is a pair/null in a `cdr` --- so that it
;;    doesn't need to be wrapped for `datum->syntax`, for example
;;
;;  * `(d->s orig-s d)` is called for each syntax object,
;;    and the second argument is result of traversing its datum
;; 
;;  * the `s-e` function extracts content of a syntax object
;;
;; The optional `seen` argument is an `eq?`-based immutable hash table
;; to detect and reject cycles. See `datum-map`.

(define (syntax-map s f d->s s-e [seen #f])
  (let loop ([s s])
    (datum-map s
               f
               (lambda (tail? v)
                 (cond
                   [(syntax? v) (d->s v (loop (s-e v)))]
                   [else (f tail? v)]))
               seen)))
