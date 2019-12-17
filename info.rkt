#lang info

(define collection "stamps")

(define deps '("base"
               "data-lib"
               "draw-lib"
               "math-lib"
               "rackunit-lib"
               "typed-racket-lib"
               "typed-racket-more"))

(define build-deps '("rackunit-lib"
                     "rackunit-typed"
                     "racket-doc"
                     "scribble-lib"))

(define compile-omit-paths '("tests"
                             "examples"
                             "profiling"))

(define pkg-desc "A language for productin art")

(define version "0.1")

(define pkg-authors '("Rodrigo Setti"))

(define scribblings '(["scribblings/stamps.scrbl" (multi-page) (language)]))

