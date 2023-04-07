#lang racket/base

(require racket/contract)
(require racket/match)

(require "./utils.rkt")

(require cbor)

(provide (struct-out plutus:data)
         (struct-out plutus:constr)
         (struct-out plutus:map)
         (struct-out plutus:list)
         (struct-out plutus:integer)
         (struct-out plutus:bytestring)
         plutus:encode-cbor
         plutus:to-data)

(struct/contract plutus:data () #:transparent) ; the supertype
(struct/contract plutus:constr plutus:data ([tag exact-integer?]
                                            [content (listof plutus:data?)]) #:transparent)
(struct/contract plutus:map plutus:data ([values (hash/c plutus:data? plutus:data?)]) #:transparent)
(struct/contract plutus:list plutus:data ([values (listof plutus:data?)]) #:transparent)
(struct/contract plutus:integer plutus:data ([value exact-integer?]) #:transparent)
(struct/contract plutus:bytestring plutus:data ([bs bytes?]) #:transparent)

(define/contract (plutus:encode-cbor data)
  (-> plutus:data? string?)
  (cbor->hex (plutus:encode-cbor~ data)))

;; When encoding plutus lists in constr, we treat the empty list specially.
(define (plutus:encode-list~ xs)
  (cond
    [(null? xs) '()]
    [else (in-list xs)]))

(define/contract (plutus:encode-cbor~ data)
  (-> plutus:data? any/c)
  (match data
    [(plutus:constr tag v)
     #:when (<= 0 tag 6)
     (cbor-tag (+ 121 tag) (plutus:encode-list~ (map plutus:encode-cbor~ v)))]
    [(plutus:constr tag v)
     #:when (<= 7 tag 127)
     (cbor-tag (+ 1273 tag) (plutus:encode-list~ (map plutus:encode-cbor~ v)))]
    [(plutus:constr tag v)
     #:when (<= 128 tag)
     (cbor-tag 102 (list tag (plutus:encode-list~ (map plutus:encode-cbor~ v))))]
    [(plutus:list xs)
     (in-list (map plutus:encode-cbor~ xs))]
    [(plutus:integer i)
     i]
    [(plutus:bytestring bs)
     bs]
    [(plutus:map ps)
     (hash-map/copy ps
                    (λ (k v) (values (plutus:encode-cbor~ k) (plutus:encode-cbor~ v))))]
    [_ (error "Could not encode cbor" data)]))

;; closed set -- not all types should be handled, users should define their own.
(define/contract (plutus:to-data a)
  (-> any/c plutus:data?)
  (match a
    [(? number? x)
     #:when (exact-integer? x)
     (plutus:integer x)]
    [(? rational? x)
     (plutus:list
        (list (plutus:integer (numerator x))
              (plutus:integer (denominator x))))]
    [(? string? str) (plutus:bytestring (string->bytes/utf-8 str))]
    [(? bytes? bs) (plutus:bytestring bs)]
    [(? list? xs) (plutus:list (map plutus:to-data xs))]
    [(? hash? h) (plutus:map (hash-map/copy h (λ (k v) (values (plutus:to-data k) (plutus:to-data v)))))]
    [(? boolean? b)
     (if b (plutus:constr 1 '())
           (plutus:constr 0 '()))]))

;; Testing
(require rackunit)

;; Encoding test through any -> plutus:data -> cbor
(define (data-encoding-test a expected-hex)
  (check-equal?
   (plutus:encode-cbor (plutus:to-data a))
   expected-hex))

;; Encoding test but taking raw plutus:data
(define (plutus-data-encoding-test a expected-hex)
  (check-equal?
   (plutus:encode-cbor a)
   expected-hex))

(data-encoding-test '#hash((2 . 4) (5 . 7)) "a202040507")
(data-encoding-test 200 "18c8")
(data-encoding-test 200000000000000000 "1b02c68af0bb140000")
(data-encoding-test -200000000000000000 "3b02c68af0bb13ffff")
(data-encoding-test 0 "00")
(data-encoding-test '(1 2 3) "9f010203ff")
(data-encoding-test '() "9fff")
(data-encoding-test #t "d87a80")
(data-encoding-test #f "d87980")
(data-encoding-test "hello, world" "4c68656c6c6f2c20776f726c64")
(data-encoding-test "" "40")
(data-encoding-test (/ 2 3) "9f0203ff")
(data-encoding-test (/ 2 5000) "9f011909c4ff")
(data-encoding-test (/ -2 5000) "9f201909c4ff")

(define constr-tests
  '((0    . "d8799f00ff")
    (4    . "d87d9f00ff")
    (7    . "d905009f00ff")
    (100  . "d9055d9f00ff")
    (1287 . "d866821905079f00ff")))
(for
  ([test constr-tests])
  (plutus-data-encoding-test
     (plutus:constr (car test) (list (plutus:integer 0)))
     (cdr test)))

(plutus-data-encoding-test (plutus:constr 20000 (list (plutus:integer 0) (plutus:integer 10)))
                           "d86682194e209f000aff")
