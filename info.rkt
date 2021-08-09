#lang info

(define name "ee-lib")
(define version "1.0")
(define deps '(["base" #:version "8.2.0.7"]
               "rackunit-lib"))
(define build-deps '("racket-doc" "scribble-lib" "drracket"))
(define scribblings '(("scribblings/ee-lib.scrbl" () (experimental))))
(define compile-omit-paths '("test/node_modules"))
(define test-omit-paths '("test/node_modules"))
