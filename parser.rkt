#lang racket/base

(provide read-term)

(define (read-term src [port (current-input-port)])
  (define-values (line column position) (port-next-location port))
  (define c (read-char port))

  (define (decorate sexp span)
    (datum->syntax #f sexp (list src line column position span)))
  
  (cond
   [(eof-object? c) eof]
   [(char-whitespace? c) (read-term src port)]
   [else
    (case c
      [(#\`)
       (let ([a (read-term src port)]
	     [b (read-term src port)])
	 (define-values (l c tail-position) (port-next-location port))
	 (decorate (list a b) (- tail-position position)))]
      [(#\")
       (let ([term (read-term src port)])
	 (define-values (l c tail-position) (port-next-location port))
	 (decorate `(%define ,(string->symbol (string (read-char port))) term)
		   (- tail-position position)))]
      [(#\.) (decorate `(%dot ,(read-char port)) 2)]
      [(#\?) (decorate `(%question ,(read-char port)) 2)]
      [(#\@) (decorate 'at 1)]
      [(#\|) (decorate 'pipe 1)]
      [(#\s) (decorate 's 1)]
      [(#\k) (decorate 'k 1)]
      [(#\i) (decorate 'i 1)]
      [(#\d) (decorate 'd 1)]
      [(#\r) (decorate `(%dot #\newline) 1)]
      [(#\c) (decorate 'c 1)]
      [(#\e) (decorate 'e 1)]
      [(#\v) (decorate 'v 1)]
      [else (error "parse error on" c)])]))
