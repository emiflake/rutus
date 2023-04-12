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
(require "bitport.rkt")

;; Constant types
(struct/contract plutus:constant () #:transparent)
(struct/contract plutus:constant-integer plutus:constant ([i exact-integer?]) #:transparent)
(struct/contract plutus:constant-bytestring plutus:constant ([bs bytes?]) #:transparent)

;; UPLC structure
(struct/contract uplc:term () #:transparent) ; the supertype
(struct/contract uplc:abs uplc:term ([arity exact-nonnegative-integer?]
                                     [body uplc:term?]) #:transparent)
(struct/contract uplc:var uplc:term ([index exact-nonnegative-integer?]) #:transparent)
(struct/contract uplc:app uplc:term ([fun uplc:term?]
                                     [arg uplc:term?]) #:transparent)
(struct/contract uplc:delay uplc:term ([arg uplc:term?]) #:transparent)
(struct/contract uplc:force uplc:term ([arg uplc:term?]) #:transparent)
(struct/contract uplc:builtin uplc:term ([builtin any/c]) #:transparent)
(struct/contract uplc:constant uplc:term ([value plutus:constant?]) #:transparent)
(struct/contract uplc:error uplc:term () #:transparent)

;; Program helpers
(struct/contract uplc:version
                 ([major exact-nonnegative-integer?]
                  [minor exact-nonnegative-integer?]
                  [patch exact-nonnegative-integer?]) #:transparent)

(struct/contract uplc:program ([version uplc:version?]
                               [body uplc:term?]) #:transparent)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flat encoding

(require bitsyntax)

;; Encoding constants

;; Encode a bytestring in blocks, prefixing each block with the block size.
(define/contract (constant-bytestring-blocks/e bs out)
  (-> bit-string? output-bitport? void?)
  (let ([block-size 255]
        [len (quotient (bit-string-length bs) 8)])
  (cond
    [(> len block-size)
     (define-values (h t) (bit-string-split-at bs (* 8 block-size)))
     (constant-bytestring-blocks/e h out)
     (constant-bytestring-blocks/e t out)]
    [else
     (bitport-write
      (bit-string
        [len :: bits 8]
        [bs :: binary]) out)])))

(define/contract (bytestring-filler/e out)
  (-> output-bitport? void?)
  (define missing (- 8 (bitport-remainder-bitcount out)))
  (cond
    ;; We take a full byte if it's already aligned
    [(= missing 0)
     (bitport-write (bit-string [1 :: bits 8]) out)]
    [(> missing 0)
     (bitport-write
      (bit-string [1 :: bits missing]) out)]))

(define/contract (constant-bytestring/e bs out)
  (-> bytes? output-bitport? void?)
  (bytestring-filler/e out)
  (constant-bytestring-blocks/e bs out)
  ;; If we are already writing an empty bytestring, then there's no need to suffix another empty block
  (unless (= 0 (bytes-length bs))
    (bitport-write (bit-string [0 :: bits 8]) out)))

(define constant-width 4)

(define (constant-tag/e t out)
  (bitport-write (bit-string [t :: bits constant-width little-endian]) out))

(define (encode-list-with/e f l out)
  (match l
    ['()
      (bitport-write (bit-string [0 :: bits 1]) out)]
    [(list-rest x xs)
      (bitport-write (bit-string [1 :: bits 1]) out)
      (f x out)
      (encode-list-with/e f xs out)]))

(define/contract (constant/e constant out)
  (-> plutus:constant? output-bitport? void?)
  (match constant
    [(plutus:constant-bytestring bs)
     (encode-list-with/e constant-tag/e '(1) out)
     (constant-bytestring/e bs out)]
    [_ (error "Unimplemented constant/e for" constant)]))

;; how wide are term tags
(define term-tag-width 4)

(define (term-tag? v)
  (and
   (exact-integer? v)
   (<= 0 v (- (expt 2 term-tag-width) 1))))

;; encode a term tag
(define/contract (term-tag/e n out)
  (-> term-tag? output-bitport? void?)
  (bitport-write (bit-string [n :: bits term-tag-width]) out))

(define/contract (term/e term out)
  (-> uplc:term? output-bitport? void?)
  (match term
    [(uplc:var i)
     (term-tag/e 0 out)
     (natural/e i out)]
    [(uplc:delay t)
     (term-tag/e 1 out)
     (term/e t out)]
    [(uplc:abs _ body)
     (term-tag/e 2 out)
     (term/e body out)]
    [(uplc:app f x)
     (term-tag/e 3 out)
     (term/e f out)
     (term/e x out)]
    [(uplc:constant con)
     (term-tag/e 4 out)
     (constant/e con out)]
    [(uplc:error)
     (bit-string
      [(term-tag/e 6) :: binary])]))

(define/contract (version/e version out)
  (-> uplc:version? output-bitport? void?)
  (match version
    [(uplc:version maj min patch)
     (natural/e maj out)
     (natural/e min out)
     (natural/e patch out)]))

(define/contract (program/e program out)
  (-> uplc:program? output-bitport? void?)
  (match program
    [(uplc:program version body)
     (version/e version out)
     (term/e body out)]))

(define/contract (uplc:encode p)
  (-> uplc:program? bytes?)
  (cbor-bytes
    (call-with-output-bytes
      (λ (out)
         (define bp (make-output-bitport out))
         (program/e p bp)
         (flat-pad/e bp)))))

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

(encoding-test (uplc:program (uplc:version 1 0 0) (uplc:abs 1 (uplc:constant (plutus:constant-bytestring #"hello"))))
               '(77 1 0 0 36 137 5 104 101 108 108 111 0 1))

(encoding-test (uplc:program
                (uplc:version 1 0 0)
                (uplc:abs 1
                          (uplc:constant
                           (plutus:constant-bytestring
                            (list->bytes (build-list 300 (λ (x) 65)))))))
               '(89 1 53 1 0 0 36 137 255 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 45 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 0 1))

(encoding-test (uplc:program (uplc:version 1 0 0) (uplc:abs 1 (uplc:constant (plutus:constant-bytestring #""))))
               '(71 1 0 0 36 137 0 1))
