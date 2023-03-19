#lang racket

(require bitsyntax)
(require racket/contract)
(require cbor)
(require rnrs/io/ports-6)

(provide
 low7
 natural/e
 integral/e
 flat-pad
 print-bits
 cbor-bytes)

(define/contract (low7 n)
  (-> exact-integer? exact-integer?)
  (bitwise-and n #x7F))

(define (w7-list t)
  (let [(l (low7 t))
        (t1 (arithmetic-shift t -7))
        (w7 (λ (l) (bitwise-ior l #x80)))]
    (cond
      [(eq? t1 0) (list l)]
      [else (cons (w7 l) (w7-list t1))])))

(define/contract (natural/e n)
  (-> exact-nonnegative-integer? bit-string?)
  (integral/e n))

(define/contract (integral/e n)
  (-> exact-integer? bit-string?)
  (let [(vs (w7-list n))]
    (integral-ws/e vs)))

(define/contract (integral-ws/e xs)
  (-> (listof exact-integer?) bit-string?)
  (for/fold
   ([bs (bit-string)])
   ([x (in-list xs)])
    (bit-string [x :: bytes 1] [bs  :: binary])))

(define/contract (flat-pad bs)
  (-> bit-string? bit-string?)
  (letrec [(padding-required (- 8 (modulo (bit-string-length bs) 8)))
           (end-padding (cond
                          [(not (= padding-required 0))
                           (bit-string [1 :: bits padding-required little-endian])]
                          [else (bit-string)]))]
    (bit-string-append bs end-padding)))

;; turn a bytestring into its cbor bytestring equivalent
(define (cbor-bytes a) 
  (call-with-bytevector-output-port 
    (λ(out)
      (cbor-write
       cbor-empty-config
       a
       out))))


(define (print-bits bs)
  (cond
     [(list? bs)
      (string-join (map (λ (b) (~r b #:base 2 #:min-width 8 #:pad-string "0")) bs) " ")]
     [(bytes? bs)
      (print-bits (bytes->list bs))]
     [(bit-string? bs)
      (print-bits (bit-string->bytes bs))]))
