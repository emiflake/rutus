#lang racket/base

(require parkt/lib/coreterm)
(require parkt/lib/uplc)
(require parkt/lib/rawterm)

(require racket/function
         (for-syntax syntax/parse
                     syntax/define
                     racket/base))

(define-syntax (core-module stx)
  (syntax-parse stx
    [(_ E ...)
     #'(#%module-begin
        (displayln "; parkt/core - 0.1.0")
        E ...
        )]))

(define-syntax (core-top-interaction stx)
  (syntax-parse stx
    [(_ E ...)
     #'(#%top-interaction
        (display (E ...))
        )]))

(define-syntax (core-top stx)
  (syntax-parse stx
    [(_ E ...)
     #'(display (E ...))]))

(define-syntax (core-app stx)
  (syntax-parse stx
    [(_ F X ...)
     #'(if (coreterm? F) (appc F X ...) (#%app F X ...))]))

(define-syntax (define-script stx)
  (syntax-parse stx
    [(define-script (NAME BINDING ...) BODY)
     #'(define-script NAME (core-lambda (BINDING ...) BODY))]
    [(define-script NAME BODY)
     #'(begin
         (display "; Compiled ")
         (displayln 'NAME)
         (define NAME (uplc:program
                       (uplc:version 1 0 0)
                       (rt->uplc 0 (coreterm-eval BODY 0))))
         (bytes->list (uplc:encode NAME)))]))


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
     #'(intro-integer (#%datum . D))]))

(define-syntax (core-+ stx)
  (syntax-parse stx
    [(_ A B)
     #'(appc (appc (intro-builtin 'AddInteger) A) B)]))


(define-syntax (core-force stx)
  (syntax-parse stx
    [(_ A)
     #'(intro-force A)]))

(define-syntax (core-delay stx)
  (syntax-parse stx
    [(_ A)
     #'(intro-delay A)]))

(define-syntax (core-if stx)
  (syntax-parse stx
    [(_ A B C)
     #'(intro-force (appc (appc (appc (intro-force (intro-builtin 'IfThenElse)) A) (intro-delay B)) (intro-delay C)))]))

(define-syntax (core-< stx)
  (syntax-parse stx
    [(_ A B)
     #'(appc (appc (intro-builtin 'LessThanInteger) A) B)]))

(define-syntax (core-* stx)
  (syntax-parse stx
    [(_ A B)
     #'(appc (appc (intro-builtin 'MultiplyInteger) A) B)]))

(define-syntax (core-- stx)
  (syntax-parse stx
    [(_ A B)
     #'(appc (appc (intro-builtin 'SubtractInteger) A) B)]))

(define-syntax (core-let-inline stx)
  (syntax-parse stx
    [(_ [(K V) ...] B)
     #'(let* [(K V) ...] B)
     ]))

(provide (rename-out [core-module #%module-begin]
                     [core-top-interaction #%top-interaction]
                     [core-top #%top]
                     [core-app #%app]
                     [core-datum #%datum])
         define-script
         (rename-out [core-lambda~ lambda~]
                     [core-lambda~ λ~])
         (rename-out [core-lambda lambda]
                     [core-lambda λ])
         (rename-out [core-let-inline let-inline])

         (rename-out [core-+ +])
         (rename-out [core-* *])
         (rename-out [core-- -])
         (rename-out [core-if if])
         (rename-out [core-< <]))
