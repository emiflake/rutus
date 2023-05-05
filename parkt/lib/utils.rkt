#lang racket/base

(provide cbor->hex
         hex->cbor)

(require racket/contract)

(require file/sha1)
(require rnrs/io/ports-6)
(require cbor)
(require racket/port)

(define/contract (cbor->hex a)
  (-> any/c string?)
  (bytes->hex-string
   (call-with-output-bytes
    (λ(out)
      (cbor-write
       (with-sorted-map-keys cbor-empty-config #t)
       a
       out)))))

(define/contract (hex->cbor hex)
  (-> string? any/c)
  (call-with-input-bytes
   (hex-string->bytes hex)
   (λ(in)
     (cbor-read
      (with-sorted-map-keys cbor-empty-config #t)
      in))))
