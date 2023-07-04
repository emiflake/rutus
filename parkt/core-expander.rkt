#lang racket/base

(require parkt/lib/coreterm)
(require parkt/lib/uplc)
(require parkt/lib/rawterm)
 
(require racket/function
         (for-syntax syntax/parse
                     syntax/define
                     racket/base))

(require syntax/parse)


(require racket/match)

(require (prefix-in lifted: racket))

(define-syntax (core-module stx)
  (syntax-parse stx
    [(_ E ...)
     #'(#%module-begin
        (displayln "; parkt/core - 0.1.0")
        E ...)]))

(define-syntax (core-top-interaction stx)
  (syntax-parse stx
    [(_ E ...)
     #'(#%top-interaction
        (E ...))]))

(define-syntax (core-top stx)
  (syntax-parse stx
    [(_ . E )
     #'(#%top . E)]))

(define-syntax (core-app stx)
  (syntax-parse stx
    [(_ F X ...)
     #'(#%app F X ...)]))

(define-syntax (define-script stx)
  (syntax-parse stx
    [(define-script (NAME BINDING ...) BODY)
     #'(define-script NAME (lambda (BINDING ...) BODY))]
    [(define-script NAME BODY)
     #'(begin
         (display "; Compiled ")
         (displayln 'NAME)
         (displayln "; Before macro: ")
         (displayln (expand #'BODY))
         (displayln "; After macro:  ")
         (println (expand #'(stx->coreterm BODY)))
         (define NAME (uplc:program (uplc:version 1 0 0) (rt->uplc 0 (coreterm-eval (stx->coreterm BODY) 0))))
         (bytes->list (uplc:encode NAME)))]))

(define-syntax (core stx)
  (syntax-parse stx
    [(_ BODY)
     #'(stx->coreterm BODY)]))

(define-syntax (define-core stx)
  (syntax-parse stx
    [(n (NAME BINDING ...) BODY)
     #'(n NAME (lambda (BINDING ...) BODY))]
    [(_ NAME BODY)
     #'(begin
         (define NAME (stx->coreterm BODY)))]))

(define-syntax (define-core-inline stx)
  (syntax-parse stx
    [(n (NAME BINDING ...) BODY)
     #'(begin
         (define (NAME BINDING ...) (core BODY)))]
    [(_ NAME BODY)
     #'(begin
         (define NAME (core BODY)))]))

;; The mother of all conversions:
;; This basically does all of the magic converting
(define-syntax (stx->coreterm stx)
  (syntax-parse stx
    #:literals [lambda #%app let]
    [(_ (lambda (ARG ...) BODY)) #'(core-lambda (ARG ...) (stx->coreterm BODY))]
    [(_ ((~literal @) X ...)) #'(X ...)]
    [(_ (~var X integer)) #'(intro-integer X)]
    [(_ (let [(K V) ...] BODY)) #'(letc* [(K (stx->coreterm V)) ...] (stx->coreterm BODY))]
    [(_ (F X ...)) #'(appc* (stx->coreterm F) (stx->coreterm X) ...)]
    [(_ A) #'A]))

(define-syntax (core-lambda~ stx)
  (syntax-parse stx
    [(_ (I) V)
     #`(intro-abs (lambda (I) V))]))

(define-syntax (core-lambda stx)
  (syntax-parse stx
    [(_ (ARG ...) BODY)
     (for/fold
      ([body #'BODY])
      ([ar (in-list (reverse (syntax->list #'(ARG ...))))])
       #`(core-lambda~ (#,ar) #,body))]))

(define-syntax (core-datum stx)
  (syntax-parse stx
    [(_ . (~var D integer))
     #'(#%datum . D)]))
; 
; (define-syntax (core-+ stx)
;   (syntax-parse stx
;     [(_ A B)
;      #'(appc (appc (intro-builtin 'AddInteger) A) B)]))
; 
; 
; (define-syntax (core-force stx)
;   (syntax-parse stx
;     [(_ A)
;      #'(intro-force A)]))
; 
; (define-syntax (core-delay stx)
;   (syntax-parse stx
;     [(_ A)
;      #'(intro-delay A)]))
; 
; (define-syntax (core-if stx)
;   (syntax-parse stx
;     [(_ A B C)
;      #'(intro-force (appc (appc (appc (intro-force (intro-builtin 'IfThenElse)) A) (intro-delay B)) (intro-delay C)))]))
; 
; (define-syntax (core-< stx)
;   (syntax-parse stx
;     [(_ A B)
;      #'(appc (appc (intro-builtin 'LessThanInteger) A) B)]))
; 
; (define-syntax (core-* stx)
;   (syntax-parse stx
;     [(_ A B)
;      #'(appc (appc (intro-builtin 'MultiplyInteger) A) B)]))
; 
; (define-syntax (core-- stx)
;   (syntax-parse stx
;     [(_ A B)
;      #'(appc (appc (intro-builtin 'SubtractInteger) A) B)]))
; 
; (define-syntax (core-let-inline stx)
;   (syntax-parse stx
;     [(_ [(K V) ...] B)
;      #'(let* [(K V) ...] B)
;      ]))


(define @ '@)

(provide (rename-out [core-module #%module-begin]
                     [core-top-interaction #%top-interaction]
                     [core-top #%top]
                     [core-app #%app]
                     [core-datum #%datum])

         define-script
         define-core
         define-core-inline
         core
         @
         lambda
         intro-builtin
         intro-force
         intro-delay
         let
         quote
         
         ;; Reexports
         (rename-out [lifted:define define]))

;; (provide (rename-out [core-module #%module-begin]
;;                      [core-top-interaction #%top-interaction]
;;                      [core-top #%top]
;;                      [core-app #%app]
;;                      [core-datum #%datum])
;;          define-script
;;          (rename-out [core-lambda~ lambda~]
;;                      [core-lambda~ λ~])
;;          (rename-out [core-lambda lambda]
;;                      [core-lambda λ])
;;          (rename-out [core-let-inline let-inline])

;;          (rename-out [core-+ +])
;;          (rename-out [core-* *])
;;          (rename-out [core-- -])
;;          (rename-out [core-if if])
;;          (rename-out [core-< <]))
