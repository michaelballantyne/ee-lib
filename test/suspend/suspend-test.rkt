(module a racket
  (#%plain-module-begin
   (#%require "suspend.rkt" rackunit)
   (#%require racket/base)
   (#%expression
    (check-equal?
     (boundary (mylanglet x (mycons x (mylanglet y (myrkt (list 5
                                                               (boundary x)))))))
     '(x 5 x)))))