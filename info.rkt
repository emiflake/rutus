#lang info
(define collection 'multi)
(define deps '("base" "cbor" "bitsyntax"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/parkt.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(emiflake przembot))
(define license '(Apache-2.0 OR MIT))
