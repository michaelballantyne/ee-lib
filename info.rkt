#lang info

(define name "ee-lib")
(define deps '("base" "rackunit-lib"))
(define build-deps '("racket-doc" "scribble-lib" "drracket"))
(define scribblings '(("scribblings/ee-lib.scrbl" () (experimental))))
(define compile-omit-paths '("test/node_modules"))
(define test-omit-paths '("test/node_modules"))
