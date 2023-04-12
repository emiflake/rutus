# parkt

Barebones Cardano Plutus utilities in Racket.

The intent of this project is to facilitate full embedding of the Plutus language in Racket and
ideally extending that to support higher abstractions and script writing with a macro-centric
approach.

If you missed macros in other languages that compile to Plutus, this is for you.

### What is currently implemented

MVP:

- [x] Barebones Plutus Data encoding in CBOR
- [ ] Builtin encoding
- [ ] UPLC Term flat encoding
- [ ] Thin macro sugar layer overtop of raw term writing; "base parkt"

Future:

- [ ] Hoisting optimizations
- [ ] Type directed programming language overtop of base parkt; "struct parkt"
- [ ] Plutus Data decoding in CBOR
- [ ] Decoding scripts
- [ ] Evaluating scripts
