#lang racket
;
; This module exposes the simple to use plutus core without any optimizations.
; It should be the compile target for another language.
;
; In particular, the following special things are to note:
; - applications and lambdas are uncurried
; - terms are represented as taking a level, which must be resolved later
; - macros:
;    - λc* for introducing lambdas
;    - letc* for introducing let bindings
;    - appc for application
;
(require "rawterm.rkt")

(provide intro-abs
         intro-app
         intro-let
         intro-integer
         intro-bytestring
         intro-builtin
         intro-force
         intro-delay
         coreterm-eval
         appc
         letc*
         appc*
         (struct-out coreterm))

(require racket/contract)

(struct coreterm (fun) #:transparent)

(define/contract (coreterm-eval coreterm i)
  (-> coreterm? exact-nonnegative-integer? rt:term?)
  ((coreterm-fun coreterm) i))


(define/contract (intro-abs f)
  (-> (-> coreterm? coreterm?) coreterm?)
  (coreterm
     (λ (i)
       (letrec
           [(v (coreterm (λ (j) (rt:var (- j (+ i 1))))))
            (t (coreterm-eval (f v) (+ i 1)))]
         (match t
           [(rt:app b [rt:var 0]) b]
           [(rt:abs n (rt:app t~ args))
            #:when
            (and #;(displayln (map (match-lambda [(rt:var v) v] [_ #f]) args))
                 (eq? (map (match-lambda [(rt:var v) v] [_ #f]) args)
                      (range 0 (+ n 1))))
            t~]
           [(rt:abs n b) (rt:abs (+ n 1) b)]
           [t (rt:abs 0 t)])))))

(define/contract (intro-force t)
  (-> coreterm? coreterm?)
  (coreterm
     (λ (i)
       (rt:force (coreterm-eval t i)))))

(define/contract (intro-delay t)
  (-> coreterm? coreterm?)
  (coreterm
     (λ (i)
       (rt:delay (coreterm-eval t i)))))

(define/contract (intro-app ft xt)
  (-> coreterm? coreterm? coreterm?)
  (coreterm
   (λ (i)
     (match* ((coreterm-eval ft i) (coreterm-eval xt i))
       [((rt:error) _) (error "applying to an error")]
       [(_ (rt:error)) (error "applying an error")]
       [((rt:abs 0 (list (rt:var 0))) y) y]
       [((rt:app f xs) y) (rt:app f (append xs (list y)))]
       [(f x) (rt:app f (list x))]))))

(define/contract (intro-let a k)
  (-> coreterm? (-> coreterm? coreterm?) coreterm?)
  (coreterm
   (λ (i)
     (match (coreterm-eval a i)
       [(rt:var _) (k a)]
       [_ (coreterm-eval (intro-app (intro-abs k) a) i)]))))

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

(define/contract (intro-integer n)
  (-> exact-integer? coreterm?)
  (coreterm (λ (_) (rt:constant (plutus:constant-integer n)))))

(define/contract (intro-bytestring s)
  (-> string? coreterm?)
  (coreterm (λ (_) (rt:constant (plutus:constant-bytestring (string->bytes/locale s))))))

(define (intro-builtin builtin)
  (-> exact-integer? coreterm?)
  (coreterm (λ (i) (rt:builtin builtin))))

(define-syntax (appc stx)
  (syntax-case stx ()
    [(_ F X)
     #`(intro-app F X)]))

(define-syntax (appc* stx)
  (syntax-case stx ()
    [(_ F ARG ...)
     (for/fold
       ([r #'F])
       ([ar (in-list (syntax->list #'(ARG ...)))])
       #`(appc #,r #,ar))]))

(define t/fst (λc* (x y) x))
(define t/snd (λc* (x y) y))

;; Encoding tests
(require "uplc.rkt")

; (let
;     [(program
;         (letc* [(id (λc* (x) x))]
;                (λc* (x y) x)))]
;   (uplc:encoding-test
;    (uplc:program
;     (uplc:version 1 0 0)
;     (rt->uplc 0 ((coreterm-fun program) 0)))
;    '(76 1 0 0 50 50 0 34 32 2 32 1 1)))

(let
    [(program
      (letc*
       [(id (λc* (i) i))
        (_fst (λc* (x _y) x))]
       id))]
  (uplc:encoding-test
   (uplc:program
    (uplc:version 1 0 0)
    (rt->uplc 0 (coreterm-eval program 0)))
   '(76 1 0 0 50 50 0 34 32 2 32 1 1)))

(let
    [(program
      (letc*
       [(id (λc* (i) i))]
       (λc* (x _y) x)))]
  (uplc:encoding-test
   (uplc:program
    (uplc:version 1 0 0)
    (rt->uplc 0 (coreterm-eval program 0)))
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
    (rt->uplc 0 (coreterm-eval program 0)))
   '(77 1 0 0 50 50 34 0 18 32 2 32 1 1)))

(let*
    [(snd (λc* (x y) y))
     (program
       (λc* (_) (appc snd (intro-integer -100000))))]
  (uplc:encoding-test
   (uplc:program
    (uplc:version 1 0 0)
    (rt->uplc 0 (coreterm-eval program 0)))
   '(75 1 0 0 35 34 0 20 130 254 104 49)))

(let*
    [(program
       (λc* (_ _ _ _ _ _ _ _ _ _) (intro-integer -100000)))]
  (uplc:encoding-test
   (uplc:program
    (uplc:version 1 0 0)
    (rt->uplc 0 (coreterm-eval program 0)))
   '(77 1 0 0 34 34 34 34 34 72 47 230 131 1)))

(let*
    [(program
      (appc
       (appc (intro-builtin 'AddInteger)
             (intro-integer 1))
       (intro-integer 1)))]
  (uplc:encoding-test
   (uplc:program
    (uplc:version 1 0 0)
    (rt->uplc 0 (coreterm-eval program 0)))
   '(74 1 0 0 51 112 9 0 18 64 5)))


; (let*
;     [(binop (λ (f) (λ (x y) (appc (appc (intro-builtin f) x) y))))
;      (add/c (binop 'AddInteger))
;      (multiply/c (binop 'MultiplyInteger))
;      (program
;       (letc
;          [(square/c (λc* (x) (multiply/c x x)))]
;          (λc* (x y) (add/c (appc square/c x) (appc square/c y)))))]
;   (uplc:encoding-test
;    (uplc:program
;     (uplc:version 1 0 0)
;     (rt->uplc 0 (coreterm-eval program 0)))
;   '(85 1 0 0 50 34 51 112 6 0 96 2 96 6 0 68 102 224 128 4 0 65)))
