#lang racket/base

(require
  json
  racket/list
  racket/system
  ee-lib/define
  (for-syntax
   racket/base
   racket/generic
   ee-lib
   syntax/stx
   syntax/id-table
   (rename-in syntax/parse [define/syntax-parse def/stx])))

(define-literal-forms js-literals
  "Javascript forms cannot be used directly as Racket expressions"
  (#%js-var
   #%js-datum
   #%js-app
   ?
   function
   set!
   +
   *
   -
   /
   <
   <=
   >
   >=
   ==

   let
   let-syntax
   return
   while
   if
   ))

(begin-for-syntax
  (define-generics js-variable-binding)
  (struct js-variable-binding-rep ()
    #:methods gen:js-variable-binding [])
  (define-generics js-macro
    (js-macro-transform js-macro stx))
  (struct js-macro-rep [transformer]
    #:methods gen:js-macro
    [(define (js-macro-transform js-macro stx)
       ((js-macro-rep-transformer js-macro) stx))])

  (define (bind-var! name)
    (bind! name #'(js-variable-binding-rep)))

  (define-syntax-class binop
    #:literal-sets (js-literals)
    (pattern (~or + * - / < <= > >= ==)))

  (define/hygienic (js-expand-expression stx) #:expression
    (syntax-parse stx
      #:literal-sets (js-literals)
      [(head:id . rest)
       #:do [(define binding (lookup #'head))]
       #:when (js-macro? binding)
       (js-expand-expression
        (js-macro-transform binding stx))]

      ; expressions; move here from generics definitions
      [(#%js-datum n:number) this-syntax]
      [(#%js-var x:id)
       (when (not (js-variable-binding? (lookup #'x)))
         (raise-syntax-error #f "unbound identifier" #'x))
       this-syntax]
      [(function (x:id ...) body ...)
       (define sc (make-scope))
       (def/stx (x^ ...)
         (for/list ([x (syntax->list #'(x ...))])
           (bind-var! (add-scope x sc))))
       (def/stx (body^ ...)
         (expand-block (add-scope #'(body ...) sc)))
       (qstx/rc (function (x^ ...) body^ ...))]
      [(#%js-app e e* ...)
       (qstx/rc (#%js-app #,(js-expand-expression #'e)
                          #,@(stx-map js-expand-expression #'(e* ...))))]
      [(? c e1 e2)
       (qstx/rc (? #,(js-expand-expression #'c)
                   #,(js-expand-expression #'e1)
                   #,(js-expand-expression #'e2)))]
      [(set! var:id e)
       #:fail-unless (js-variable-binding? (lookup #'var)) (format "expected variable")
       (qstx/rc (set! var #,(js-expand-expression #'e)))]
      [(op:binop e1 e2)
       (qstx/rc (op #,(js-expand-expression #'e1) #,(js-expand-expression #'e2)))]

      ; implicits / interposition points
      [x:id
       (with-syntax ([var (datum->syntax stx '#%js-var)])
         (js-expand-expression (qstx/rc (var x))))]
      [n:number
       (with-syntax ([datum (datum->syntax stx '#%js-datum)])
         (js-expand-expression (qstx/rc (datum n))))]
      [(e ...)
       (with-syntax ([app (datum->syntax stx '#%js-app)])
         (js-expand-expression (qstx/rc (app e ...))))]
      
      [else
       (raise-syntax-error #f "not a js expression" stx)]))

  (define/hygienic (js-expand-statement-pass1 stx) #:definition
    (syntax-parse stx
      #:literal-sets (js-literals)
      [(head . rest)
       #:do [(define binding (lookup #'head))]
       #:when (js-macro? binding)
       (js-expand-statement-pass1
        (js-macro-transform binding stx))]

      ; statements
      [(let x:id e)
       (def/stx x^ (bind-var! #'x))
       (qstx/rc (let x^ e))]
      [(let-syntax m:id e)
       (def/stx m^ (bind! #'m #'(js-macro-rep e)))
       #'(let-syntax m^ e)]
      [(return e) this-syntax]
      [(while condition body ...) this-syntax]
      [(if c (b1 ...) (b2 ...)) this-syntax]
      
      ; Assume it's an expression; we'll expand those in pass 2.
      [_ stx]))

  (define/hygienic (js-expand-statement-pass2 stx) #:definition
    (syntax-parse stx
      #:literal-sets (js-literals)
      ; statements
      [(let x:id e)
       (qstx/rc (let x #,(js-expand-expression #'e)))]
      [(let-syntax m:id e) this-syntax]
      [(return e)
       (qstx/rc (return #,(js-expand-expression #'e)))]
      [(while condition body ...)
       (qstx/rc (while #,(js-expand-expression #'condition)
                       #,@(expand-block #'(body ...))))]
      [(if c (b1 ...) (b2 ...))
       (qstx/rc (if #,(js-expand-expression #'c)
                    #,(expand-block #'(b1 ...))
                    #,(expand-block #'(b2 ...))))]
  
      [_ (js-expand-expression stx)]))

  (define (expand-block body)
    (define sc (make-scope))
    (define body^
      (for/list ([b (syntax->list body)])
        (js-expand-statement-pass1 (add-scope b sc))))
    (for/list ([b body^])
      (js-expand-statement-pass2 b)))

  ; Compilation to JS
  (define (extract-js-expression stx)
    (syntax-parse stx
      #:literal-sets (js-literals)
      ; expressions
      [(#%js-datum n:number)
       (hasheq
        'type "Literal"
        'value (syntax->datum #'n))]
      [(#%js-var x:id)
       (extract-ref #'x)]
      [(function (x:id ...) body ...)
       (hasheq
        'type "FunctionExpression"
        'params (stx-map (λ (x) (extract-binder x)) #'(x ...))
        'body (extract-block #'(body ...)))]
      [(#%js-app e e* ...)
       (hasheq
        'type "CallExpression"
        'callee (extract-js-expression #'e)
        'arguments (stx-map extract-js-expression #'(e* ...)))]
      [(? c e1 e2)
       (hasheq
        'type "ConditionalExpression"
        'test (extract-js-expression #'c)
        'consequent (extract-js-expression #'e1)
        'alternate (extract-js-expression #'e2))]
      [(set! var:id e)
       (hasheq
        'type "AssignmentExpression"
        'operator "="
        'left (extract-ref #'var)
        'right (extract-js-expression #'e))]
      [(op:binop e1 e2)
       (hasheq
        'type "BinaryExpression"
        'operator (symbol->string (syntax->datum #'op))
        'left (extract-js-expression #'e1)
        'right (extract-js-expression #'e2))]
      
      [_ ; If it didn't match as a statement, it should be an expression
       (hash
        'type "ExpressionStatement"
        'expression (extract-js-expression stx))]
      ))
  
  (define (extract-js-statement stx)
    (syntax-parse stx
      #:literal-sets (js-literals)
      ; statements
      [(let x:id e)
       (hasheq
        'type "VariableDeclaration"
        'kind "let"
        'declarations
        (list (hasheq
               'type "VariableDeclarator"
               'id (extract-binder #'x)
               'init (extract-js-expression #'e))))]
      [(let-syntax m:id e)
       (hasheq 'type "EmptyStatement")]
      [(return e)
       (hasheq
        'type "ReturnStatement"
        'argument (extract-js-expression #'e))]
      [(while condition body ...)
       (hasheq
        'type "WhileStatement"
        'test (extract-js-expression #'condition)
        'body (extract-block #'(body ...)))]
      [(if c (b1 ...) (b2 ...))
       (hasheq
        'type "IfStatement"
        'test (extract-js-expression #'c)
        'consequent (extract-block #'(b1 ...))
        'alternate (extract-block #'(b2 ...)))]
      
      [_ ; if it wasn't one of the statements, it should be an expression
       (hash
        'type "ExpressionStatement"
        'expression (extract-js-expression stx))]
      ))

  (struct idmap (table [ctr #:mutable]))
  (define current-idmap (make-parameter #f))
   
  (define (do-extract stx)
    (parameterize ([current-idmap (idmap (make-free-id-table) 0)])
      (extract-js-expression stx)))

  (define (extract-binder id)
    (define map (current-idmap))
    (when (free-id-table-ref! (idmap-table map) id (lambda () #f))
      (raise-syntax-error #f "duplicate binding occurance" id))
    (define name (string-append (symbol->string (syntax->datum id))
                                (number->string (idmap-ctr map))))
    (free-id-table-set! (idmap-table map) id name)
    (set-idmap-ctr! map (+ (idmap-ctr map) 1))
    (hasheq 'type "Identifier" 'name name))
  
  (define (extract-ref id)
    (define name (free-id-table-ref! (idmap-table (current-idmap)) id
                                     (lambda () (raise-syntax-error #f "unbound identifier" id))))
    (hasheq 'type "Identifier" 'name name))

  (define (extract-block body)
    (hasheq
     'type "BlockStatement"
     'body (stx-map (λ (b) (extract-js-statement b))
                    body))))

(define (runjs estree)
  (define f (fifth (process*/ports
                    (current-output-port)
                    (open-input-string (jsexpr->string estree))
                    (current-error-port)
                    (find-executable-path "node")
                    "runjs.js")))
  (f 'wait)
  (void))

(define-syntax js
  (syntax-parser
    [(_ arg)
     (ee-lib-boundary
         (def/stx expanded-js (js-expand-expression #'arg))
       (def/stx extracted (do-extract #'expanded-js))
       #'(begin
           (define wrapped (hash 'type "ExpressionStatement" 'expression 'extracted))
           ;(pretty-display wrapped)
           (runjs wrapped)))]))

(define-syntax-rule
  (define-js-macro name e)
  (define-syntax name (js-macro-rep e)))

(define-js-macro cond
  (syntax-parser
    #:literals (else)
    [(cond [else e])
     #'e]
    [(cond [c e] clause ...)
     #'(? c e (cond clause ...))]))

(define-js-macro inc!
  (syntax-parser
    [(_ v:id)
     #'(set! v (+ 1 v))]))

(define-js-macro defn
  (syntax-parser
    [(_ (name:id args:id ...) body ...)
     #'(let name (function (args ...) body ...))]))


(module+ test
  (js ((function ()
                 (let factorial (function (n)
                                          5 ; expressions are allowed in statement position
                                          (if (<= n 1)
                                              ((return 1))
                                              ((return (* n (factorial (- n 1))))))))
                 (return (factorial 5)))))
  ; Thought this was broken due to expander bug, but doesn't seem to be...
  #;(js ((function ()
                   (defn (factorial n)
                     (return (? (<= n 1) 1 (* n (factorial (- n 1))))))
                   (return (factorial 5)))))
  

  (js ((function ()
                 (let factorial (function (n)
                                          (let i 1)
                                          (let res 1)
                                          (while (<= i n)
                                                 (set! res (* res i))
                                                 (inc! i))
                                          (return res)))
                 (return (factorial 5)))))

  (js ((function ()
                 (let fib (function (n)
                                    (return
                                     (cond
                                       [(== n 1) 1]
                                       [(== n 2) 1]
                                       [else (+ (fib (- n 1)) (fib (- n 2)))]))))
                 (return (fib 6)))))

  ; A macro defined inside the langauge. Also a use-site binder test.
  (js ((function ()
                 (let x 5)
                 (let-syntax m (lambda (stx)
                                 (syntax-parse stx
                                   [(_ arg)
                                    #'((function (arg) (return x)) 6)])))
                 (return (m x)))))

  ; Same as previous, but at statement rather than expression position.
  (js ((function ()
                 (let x 5)
                 (let-syntax m (lambda (stx)
                                 (syntax-parse stx
                                   [(_ arg)
                                    #'(return ((function (arg) (return x)) 6))])))
                 (inc! x)
                 (m x))))
  
  )
