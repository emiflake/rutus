#lang racket

(require bitsyntax)

(bit-string-pack (bit-string [1 :: bits 1] [0 :: bits 1] [4 :: bits 6]))
