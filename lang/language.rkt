#lang racket/base

(define current-character (make-parameter #f))

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
(define (at x)
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

(define (pipe x)
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

(define-syntax-rule (unlambda-module-begin body ...)
  (#%plain-module-begin
   (parameterize ([current-character #f])
     (void)
     body ...)))

(provide s k i d e v c r at pipe %dot %question #%datum
         (rename-out [unlambda-app #%app]
                     [unlambda-module-begin #%module-begin]))
