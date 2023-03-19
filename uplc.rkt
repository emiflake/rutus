#lang racket

(require racket/contract)

(require cbor)

(provide
 (struct-out uplc:term)

 (struct-out uplc:abs)
 (struct-out uplc:var)
 (struct-out uplc:app)
 (struct-out uplc:error)

 (struct-out uplc:version)
 (struct-out uplc:program)

 uplc:encode
 (prefix-out uplc: program/e)
 (prefix-out uplc: encoding-test))


(require bitsyntax)
(require "enc.rkt")

(struct/contract uplc:term () #:transparent) ; the supertype
(struct/contract uplc:abs uplc:term ([arity exact-nonnegative-integer?]
                                     [body uplc:term?]) #:transparent)
(struct/contract uplc:var uplc:term ([index exact-nonnegative-integer?]) #:transparent)
(struct/contract uplc:app uplc:term ([fun uplc:term?]
                                     [arg uplc:term?]) #:transparent)
(struct/contract uplc:delay uplc:term ([arg uplc:term?]) #:transparent)
(struct/contract uplc:force uplc:term ([arg uplc:term?]) #:transparent)
(struct/contract uplc:builtin uplc:term ([builtin any/c]) #:transparent)
(struct/contract uplc:error uplc:term () #:transparent)

(struct/contract uplc:version
                 ([major exact-nonnegative-integer?]
                  [minor exact-nonnegative-integer?]
                  [patch exact-nonnegative-integer?]) #:transparent)

(struct/contract uplc:program ([version uplc:version?]
                               [body uplc:term?]) #:transparent)


;; Flat encoding

(require bitsyntax)

;; how wide are term tags
(define term-tag-width 4)

(define (term-tag? v)
  (and
   (exact-integer? v)
   (<= 0 v (- (expt 2 term-tag-width) 1))))

;; encode a term tag
(define/contract (term-tag/e n)
  (-> term-tag? bit-string?)
  (bit-string [n :: bits term-tag-width]))

(define/contract (term/e term)
  (-> uplc:term? bit-string?)
  (match term
    [(uplc:var i)
     (bit-string-append
      (term-tag/e 0)
      (natural/e i))]
    [(uplc:delay t)
     (bit-string-append
      (term-tag/e 1)
      (term/e t))]
    [(uplc:abs _ body)
     (bit-string-append
      (term-tag/e 2)
      (term/e body))]
    [(uplc:app f x)
     (bit-string-append
      (term-tag/e 3)
      (term/e f)
      (term/e x))]
    [(uplc:error)
     (bit-string
      [(term-tag/e 6) :: binary])]))

(define/contract (version/e version)
  (-> uplc:version? bit-string?)
  (match version
    [(uplc:version maj min patch)
     (bit-string
      [(natural/e maj) :: binary]
      [(natural/e min) :: binary]
      [(natural/e patch) :: binary])]))

(define/contract (program/e program)
  (-> uplc:program? bit-string?)
  (match program
    [(uplc:program version body)
     (bit-string
      [(version/e version) :: binary]
      [(term/e body) :: binary])]))


(define/contract (uplc:encode p)
  (-> uplc:program? bytes?)
  (cbor-bytes
   (bit-string->bytes
    (flat-pad (program/e p)))))


(require rackunit)

(define (encoding-test t expected-bytes)
  (check-equal?
     (bytes->list (uplc:encode t))
     expected-bytes))

(encoding-test (uplc:program (uplc:version 1 0 0) (uplc:abs 1 (uplc:var 1)))
               '(70 1 0 0 32 1 1))

(encoding-test (uplc:program (uplc:version 1 0 0) (uplc:abs 1 (uplc:delay (uplc:var 1))))
               '(70 1 0 0 33 0 17))

(encoding-test (uplc:program (uplc:version 1 0 0) (uplc:abs 1 (uplc:abs 1 (uplc:var 2))))
               '(70 1 0 0 34 0 33))

(encoding-test (uplc:program (uplc:version 1 0 0) (uplc:abs 1 (uplc:abs 1 (uplc:var 1))))
               '(70 1 0 0 34 0 17))

