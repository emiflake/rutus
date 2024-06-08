#lang parkt/core

(define-core + (@ intro-builtin 'AddInteger))
(define-core * (@ intro-builtin 'MultiplyInteger))

(define-core (square x) (* x x))

(define-core-inline (sum-of-squares x y)
  (let [(square~ square)] (+ (square~ x) (square~ y))))

(define-script (example-script a)
  (@ sum-of-squares (core 10) (core 20)))
