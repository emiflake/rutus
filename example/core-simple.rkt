#lang rutus/core

(define (if condition t f)
  (@ intro-force (core ((@ intro-builtin 'IfThenElse) condition (@ intro-delay t) (@ intro-delay f)))))

(define < (@ intro-builtin 'LessThanInteger))

(define + (@ intro-builtin 'AddInteger))
(define - (@ intro-builtin 'SubtractInteger))
(define * (@ intro-builtin 'MultiplyInteger))

(define (square x) (* x x))

(define (fix f) ((lambda (x) (f (lambda (v) ((x x) v))))
                      (lambda (x) (f (lambda (v) ((x x) v))))))

(define (iterate-n n f x)
  ((fix (lambda (self n f x)
           (if (< n 1)
               x
               (self (- n 1) f (f x)))))
   n f x))

(define iterate-n2
  (fix (lambda (self n f x)
               (if (< 1 n)
                   x
                   (self (- n 1) f (f x))))))

(define foo "hi")

(define bytestring-length (@ intro-builtin 'LengthOfByteString))

(define-script
  string-example
  (bytestring-length foo))

(define-script
  example
  (iterate-n 5 square 2))

(define-script
   example2
  (iterate-n2 5 square 2))


