#lang racket/base

(require bitsyntax)
(require "enc.rkt")
(require "bitport.rkt")

(provide
 plutus:builtins
 plutus:builtin?
 plutus:builtin-tag
 plutus:builtin-tag/e)

(require racket/contract)
(require racket/match)

(define plutus:builtins
  #hash(
    ; Integers
    (AddInteger . 0)
    (SubtractInteger . 1)
    (MultiplyInteger . 2)
    (DivideInteger . 3)
    (QuotientInteger . 4)
    (RemainderInteger . 5)
    (ModInteger . 6)
    (EqualsInteger . 7)
    (LessThanInteger . 8)
    (LessThanEqualsInteger . 9)
    ; Bytestrings
    (AppendByteString . 10)
    (ConsByteString . 11)
    (SliceByteString . 12)
    (LengthOfByteString . 13)
    (IndexByteString . 14)
    (EqualsByteString . 15)
    (LessThanByteString . 16)
    (LessThanEqualsByteString . 17)
    ; Cryptography and hashes
    (Sha2_256 . 18)
    (Sha3_256 . 19)
    (Blake2b_256 . 20)
    (VerifyEd25519Signature . 21)
    (VerifyEcdsaSecp256k1Signature . 52)
    (VerifySchnorrSecp256k1Signature . 53)
    ; Strings
    (AppendString . 22)
    (EqualsString . 23)
    (EncodeUtf8 . 24)
    (DecodeUtf8 . 25)
    ; Bool
    (IfThenElse . 26)
    ; Unit
    (ChooseUnit . 27)
    ; Tracing
    (Trace . 28)
    ; Pairs
    (FstPair . 29)
    (SndPair . 30)
    ; Lists
    (ChooseList . 31)
    (MkCons . 32)
    (HeadList . 33)
    (TailList . 34)
    (NullList . 35)
    ; Data
    (ChooseData . 36)
    (ConstrData . 37)
    (MapData . 38)
    (ListData . 39)
    (IData . 40)
    (BData . 41)
    (UnConstrData . 42)
    (UnMapData . 43)
    (UnListData . 44)
    (UnIData . 45)
    (UnBData . 46)
    (EqualsData . 47)
    (SerialiseData . 48)
    ; Misc monomorphized constructors.
    (MkPairData . 49)
    (MkNilData . 50)
    (MkNilPairData . 51)))

(define (plutus:builtin? b)
  (not (eq? (hash-ref plutus:builtins b #f) #f)))

(define/contract (plutus:builtin-tag b)
  (-> plutus:builtin? exact-nonnegative-integer?)
  (hash-ref plutus:builtins b))

(define builtin-width 7)

(define/contract (plutus:builtin-tag/e builtin out)
  (-> plutus:builtin? output-bitport? void?)
  (bitport-write (bit-string [(plutus:builtin-tag builtin) :: bits builtin-width]) out))
