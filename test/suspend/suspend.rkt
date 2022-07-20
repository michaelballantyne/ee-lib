#lang racket

(provide boundary myrkt mycons mylanglet)

(require
  "../../define.rkt"
  (for-syntax
   racket
   racket/pretty
   syntax/id-table
   (rename-in syntax/parse [define/syntax-parse def/stx])
   "../../main.rkt"))

(define-literal-forms
  mylang-literals
  "can't use in Racket"
  (mylanglet myrkt mycons))

(begin-for-syntax
  (struct mylangbinding [])

  ; new sealing behavior breaks this hacky suspension impl, so use
  ; #:definition to avoid. (Wrong use-site hygiene for the moment, of course)
  (define/hygienic (my-expand stx) #:definition
    (syntax-parse stx
      #:literal-sets (mylang-literals)
      [(mylanglet v b)
       (with-scope s
         (def/stx x^ (bind! (add-scope #'v s) (mylangbinding)))
         ;(pretty-print (syntax-debug-info (flip-intro-scope #'x^)))
         (def/stx b^ (my-expand (add-scope #'b s)))
         (qstx/rc (mylanglet x^ b^)))]
      [(mycons e1 e2)
       (def/stx e1^ (my-expand #'e1))
       (def/stx e2^ (my-expand #'e2))
       (qstx/rc (mycons e1^ e2^))]
      [x:id
       (define b (lookup #'x))
       (unless (mylangbinding? b)
         (raise-syntax-error #f "unbound or wrong kind of binding" #'x))
       #'x]
      [(myrkt e (~optional _))
       (qstx/rc (myrkt e #,(current-def-ctx)))]))
  
  (define (my-compile stx)
    (syntax-parse stx
      #:literal-sets (mylang-literals)
      [(mylanglet v b)
       (def/stx v^ (compile-binder! #'v))
       #`(let ([v^ 'v]) #,(my-compile #'b))]
      [(mycons e1 e2)
       #`(cons #,(my-compile #'e1) #,(my-compile #'e2))]
      [x:id
       (compile-reference #'x)]
      [(myrkt e ctx)
       #'(resumption e ctx)]
      )))

(define-syntax (resumption stx)
  (syntax-parse stx
    [(_ e ctx)
     ;(displayln 'inresumption)
     (define res 
       (local-expand #'e 'expression '() (list (syntax-local-make-definition-context)
                                               (syntax->datum #'ctx))))
     ;(displayln 'resumptionres)
     ;(displayln res)
     res
     ])
  )

(define-syntax (boundary stx)
  (syntax-parse stx
    [(_ e)
     (define e^ (my-expand #'e))
     ;(displayln e^)
     (define e^^ (my-compile e^))
     ;(displayln 'boundarycompiled)
     ;(displayln e^^)
     e^^]))

