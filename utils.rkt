#lang racket/base

(provide cbor->hex
         hex->cbor)

(require racket/contract)

(require file/sha1)
(require rnrs/io/ports-6)
(require cbor)
(require racket/port)

(define (cbor->hex a)
  (bytes->hex-string
   (call-with-bytevector-output-port
    (λ(out)
      (cbor-write
       (with-sorted-map-keys cbor-empty-config #t)
       a
       out)))))

(define (hex->cbor hex)
  (call-with-input-bytes
   (hex-string->bytes hex)
   (λ(in)
     (cbor-read
      (with-sorted-map-keys cbor-empty-config #t)
      in))))
