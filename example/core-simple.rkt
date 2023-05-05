#lang parkt/core

(define-script
   example
   (let-inline
    [(square (lambda (x) (* x x)))
     (fix (lambda (f)
            ((lambda (x) (f (lambda (v) ((x x) v))))
             (lambda (x) (f (lambda (v) ((x x) v)))))))
     (iterate-n (lambda (n f x)
                  ((((fix
                      (lambda (self n f x)
                         (if (< n 1)
                             x
                             (((self (- n 1)) f) (f x)))))
                     n) f) x)))
     (ex (((iterate-n 5) square) 2))]
    ex))
