#lang racket/base

(require racket/contract)
(require racket/match)

(require "./utils.rkt")


(provide (struct-out plutus:data)
         (struct-out plutus:constr)
         (struct-out plutus:map)
         (struct-out plutus:list)
         (struct-out plutus:integer)
         (struct-out plutus:bytestring))

(struct/contract plutus:data () #:transparent) ; the supertype
(struct/contract plutus:constr plutus:data ([tag exact-integer?]
                                            [content plutus:data?]) #:transparent)
(struct/contract plutus:map plutus:data ([values (hash/c plutus:data? plutus:data?)]) #:transparent)
(struct/contract plutus:list plutus:data ([values (listof plutus:data?)]) #:transparent)
(struct/contract plutus:integer plutus:data ([value exact-integer?]) #:transparent)
(struct/contract plutus:bytestring plutus:data ([bs bytes?]) #:transparent)

(define/contract (plutus:to-data a)
  (-> any/c plutus:data?)
  (match a
    [(? number? x) (plutus:integer x)]
    [(? string? str) (plutus:bytestring (string->bytes/utf-8 str))]
    [(? bytes?  bs) (plutus:bytestring bs)]
    [(? list?   xs) (plutus:list (map plutus:to-data xs))]))
