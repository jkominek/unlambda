#lang racket/base

(require (for-syntax racket/base
		     syntax/strip-context)
	 racket/contract)

(define current-character (make-parameter #f))

(define one-arg-lambda
  (recursive-contract (-> one-arg-lambda one-arg-lambda)))

; d is special and receives its first argument as a thunk
(define ((d x) y)
  (unlambda-app (x) y))
(define (i x)
  x)
(define (c x)
  (call/cc
   (lambda (continuation)
     (unlambda-app x (procedure-reduce-arity continuation 1)))))
(define (e x)
  (exit))
(define (v x)
  v)
(define (((s x) y) z)
  (unlambda-app (unlambda-app x z) (unlambda-app y z)))
(define ((k x) y)
  x)
(define (%dot c)
  (lambda (x)
    (write-char c)
    x))
(define r (%dot #\newline))
(define (@ x)
  (let ([c (read-char)])
    (current-character c)
    (unlambda-app
     x
     (if (eof-object? c)
	 v
	 i))))

(define (%question c)
  (lambda (x)
    (unlambda-app
     x
     (if (equal? c (current-character))
	 i
	 v))))

(define (\| x)
  (unlambda-app
   x
   (if (char? (current-character))
       (%dot (current-character))
       v)))

(define (applyer function thunk)
  (if (equal? function d)
      (d thunk)
      (function (thunk))))

(define-syntax-rule (unlambda-app x y)
  (#%plain-app applyer x (lambda () y)))

(define-syntax (unlambda-module-begin stx)
  (syntax-case stx ()
    [(_ )
     #`(#%module-begin
	(module configure-runtime racket/base
	 (#%require unlambda/parser)
	 (current-read-interaction read-term))
	(void))]
    [(_ body ...)
     (let ([a-d-o (replace-context (car (syntax-e #'(body ...)))
				   #'(all-defined-out))])
       #`(#%module-begin
	  (module configure-runtime racket/base
	   (#%require unlambda/parser)
	   (current-read-interaction read-term))
	  body ...
	  (provide #,a-d-o)))]))

(define-syntax-rule (unlambda-define name value)
  (define/contract name one-arg-lambda (procedure-rename value 'name)))

(provide s k i d e v c r @ \| %dot %question #%datum all-defined-out
	 #%top-interaction
         (rename-out [unlambda-app #%app]
                     [unlambda-module-begin #%module-begin]
		     [unlambda-define %define]))
