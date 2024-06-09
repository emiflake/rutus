#lang racket/base
; A very simple port-like interface for emitting bit strings into
; a port. It keeps track of a 0-7 bits until they form a full byte
; once they do, they are emitted.
;
; The main purpose of this is for emitting streams of bits without
; being inefficient in keeping them around. We additionally get the
; benefit of knowing the current alignment status, using
; `bitport-remainder-bitcount`.

(require racket/contract)
(require bitsyntax)

;; This could maybe be marginally optimized if we avoid using bit-string as a buffer
(struct/contract output-bitport ([port output-port?] [left-overs bit-string?]) #:transparent #:mutable)

(define/contract (bitport-write bits out)
  (-> bit-string? output-bitport? void?)
  (define new-bits (bit-string-append (output-bitport-left-overs out) bits))
  (define new-len (bit-string-length new-bits))
  (cond
    [(>= new-len 8)
     (define-values (h t) (bit-string-split-at new-bits (* (quotient new-len 8) 8)))
     (write-bytes (bit-string->bytes h) (output-bitport-port out))
     (set-output-bitport-left-overs! out t)]
    [else (set-output-bitport-left-overs! out new-bits)]))

(define/contract (bitport-remainder-bitcount out)
  (-> output-bitport? exact-nonnegative-integer?)
  (bit-string-length (output-bitport-left-overs out)))

(define/contract (make-output-bitport port)
  (-> output-port? output-bitport?)
  (output-bitport port (bit-string)))

(provide
   (struct-out output-bitport)
   make-output-bitport
   bitport-write
   bitport-remainder-bitcount)
