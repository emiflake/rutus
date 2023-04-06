#lang racket

;;
;; This module exposes the simple to use plutus core without any optimizations.
;; It should be the compile target for another language.
;;
;; In particular, the following special things are to note:
;; - applications and lambdas are uncurried
;; - terms are represented as taking a level, which must be resolved later
;; - macros:
;;    - λc* for introducing lambdas
;;    - letc* for introducing let bindings
;;    - appc for application
;;
(require "rawterm.rkt")

(define (intro-abs f)
  (λ (i)
    (letrec
        [(v (λ (j) (rt:var (- j (+ i 1)))))
         (t ((f v) (+ i 1)))]
      (match t
        [(rt:app b [rt:var 0]) b]
        [(rt:abs n b) (rt:abs (+ n 1) b)]
        [t (rt:abs 0 t)]))))

(define (intro-app ft xt)
  (λ (i)
    (match* ((ft i) (xt i))
      [((rt:error) _) (error "applying to an error")]
      [(_ (rt:error)) (error "applying an error")]
      [((rt:abs 0 (list (rt:var 0))) y) y]
      [((rt:app f xs) y) (rt:app f (cons y xs))]
      [(f x) (rt:app f (list x))])))

(define (intro-let a k)
  (λ (i)
    (match (a i)
      [(rt:var _) (k a)]
      [_ ((intro-app (intro-abs k) a) i)])))

(define-syntax letc
  (syntax-rules ()
    [(letc [(I V)] B)
     (intro-let V (λ (I) B))]))

(define-syntax (letc* stx)
  (syntax-case stx ()
    [(_ [(ID VAL) ...] BODY)
     (for/fold
      ([body #'BODY])
      ([binding (in-list (reverse (syntax->list #'((ID VAL) ...))))])
       #`(letc [#,binding] #,body))]))

(define-syntax λc
  (syntax-rules ()
    [(λc (I) V)
     (intro-abs (λ (I) V))]))

(define-syntax (λc* stx)
  (syntax-case stx ()
    [(_ (ARG ...) BODY)
     (for/fold
      ([body #'BODY])
      ([ar (in-list (reverse (syntax->list #'(ARG ...))))])
       #`(λc (#,ar) #,body))]))

(define-syntax (appc stx)
  (syntax-case stx ()
    [(_ F X)
     #`(intro-app F X)]))

(define t/fst (λc* (x y) x))
(define t/snd (λc* (x y) y))


;; Encoding tests

(require "uplc.rkt")

(let
    [(program
      (letc*
       [(id (λc* (i) i))
        (_fst (λc* (x _y) x))]
       id))]
  (uplc:encoding-test
   (uplc:program
    (uplc:version 1 0 0)
    (rt->uplc 0 (program 0)))
   '(76 1 0 0 50 50 0 34 32 2 32 1 1)))

(let
    [(program
      (letc*
       [(id (λc* (i) i))]
       (λc* (x _y) x)))]
  (uplc:encoding-test
   (uplc:program
    (uplc:version 1 0 0)
    (rt->uplc 0 (program 0)))
   '(73 1 0 0 50 34 0 34 0 17)))

(let
    [(program
      (letc*
       [(id (λc* (i) i))
        (fst (λc* (x _y) x))]
       (λc* (_x y) y)))]
  (uplc:encoding-test
   (uplc:program
    (uplc:version 1 0 0)
    (rt->uplc 0 (program 0)))
   '(77 1 0 0 50 50 34 0 18 32 2 32 1 1)))
