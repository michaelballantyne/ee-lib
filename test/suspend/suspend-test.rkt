(module a racket
  (#%module-begin
   (#%require "suspend.rkt")
   (#%require racket/base)
   list
   (#%expression
    (boundary (mylanglet x (mycons x (mylanglet y (myrkt (list 5
                                                               (boundary x))))))))))