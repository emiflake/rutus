#lang racket

(require racket/contract)

(require racket/pretty)

(provide
 rt:abs
 rt:var
 rt:app
 rt:delay
 rt:force
 rt:builtin
 rt:eval
 rt:error
 rt->uplc)

(define-syntax introduce-constructors
  (syntax-rules ()
    [(introduce-constructors
      [NAME (ARG ...)] ...)
     (begin
       (struct NAME (ARG ...) #:transparent)
       ...)]))

(struct/contract rt:term () #:transparent) ; the supertype
(struct/contract rt:abs rt:term ([arity exact-nonnegative-integer?]
                                 [body rt:term?]) #:transparent)
(struct/contract rt:var rt:term ([index exact-nonnegative-integer?]) #:transparent)
(struct/contract rt:app rt:term ([fun rt:term?]
                                 [arg (listof rt:term?)]) #:transparent)
(struct/contract rt:delay rt:term ([arg rt:term?]) #:transparent)
(struct/contract rt:force rt:term ([arg rt:term?]) #:transparent)
(struct/contract rt:builtin rt:term ([builtin any/c]) #:transparent)
(struct/contract rt:error rt:term () #:transparent)

(struct/contract rt:version
                 ([major exact-nonnegative-integer?]
                  [minor exact-nonnegative-integer?]
                  [patch exact-nonnegative-integer?]) #:transparent)

(struct/contract rt:program ([version rt:version?]
                             [body rt:term?]) #:transparent)

;; toy eval fn
(define/contract (rt:eval term)
  (-> rt:term? rt:term?)
  (match term
    [(rt:app (rt:abs i b) xs)
     (app-subst* i xs b)]
    [(rt:var idx) (error "should never happen")]
    [t t]))

(define/contract (app-subst* i args b)
  (-> exact-nonnegative-integer? (listof rt:term?) rt:term? rt:term?)
  (for/fold
   ([body b]
    [arity i]
    #:result body)
   ([argument (in-list args)])
    (values
     (app-subst arity argument body)
     (- arity 1))))

(define/contract (app-subst i v body)
  (-> exact-nonnegative-integer? rt:term? rt:term? rt:term?)
  (match body
    [(rt:var idx) #:when (eq? idx i) v]
    [(rt:abs arity body) (rt:abs arity (app-subst i v body))]
    [t t]))

;; sanity tests
(require rackunit)

(check-equal? (rt:eval (rt:app (rt:abs 0 (rt:var 0)) (list (rt:abs 0 (rt:var 0)))))
              (rt:abs 0 (rt:var 0)))


;; conversion to uplc

(require "uplc.rkt")

(define/contract (rt->uplc level term)
  (-> exact-nonnegative-integer? rt:term? uplc:term?)
  (match term
    [(rt:var i) (uplc:var (+ i 1))]
    [(rt:abs arity body)
     (for/fold
      ([b (rt->uplc (+ level arity 1) body)])
      ([l (in-range 0 (+ arity 1))])
       (uplc:abs 0 b))]
    [(rt:app f xs)
     (for/fold
      ([b (rt->uplc level f)])
      ([x (in-list xs)])
       (uplc:app b (rt->uplc level x)))]
    [(rt:error)
     (uplc:error)]))


