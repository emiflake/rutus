#lang parkt/core

(define-core + (@ intro-builtin 'AddInteger))
(define-core * (@ intro-builtin 'MultiplyInteger))
(define-core - (@ intro-builtin 'SubtractInteger))

(define-core (if condition t f)
  (@ intro-force (core ((@ intro-builtin 'IfThenElse) condition (@ intro-delay t) (@ intro-delay f)))))

(define-core < (@ intro-builtin 'LessThanInteger))

(define-core (square x) (* x x))

(define-core (fix f) ((lambda (x) (f (lambda (v) ((x x) v)))) (lambda (x) (f (lambda (v) ((x x) v))))))

(define-core (iterate-n n f x)
  ((fix (lambda (self n f x)
           (if (< n 1)
               x
               (self (- n 1) f (f x)))))
   n f x))

(define-core iterate-n2
  (fix (lambda (self n f x)
               (if (< n 1)
                   x
                   (self (- n 1) f (f x))))))

(define-script
   example
  (iterate-n 5 square 2))

(define-script
   example2
  (iterate-n2 5 square 2))


