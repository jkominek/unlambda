#lang racket/base

(provide read-term color-lexer)

(define (color-lexer port)
  (define-values (line column position) (port-next-location port))
  (define c (read-char port))

  (cond
   [(eof-object? c)
    (values #f 'eof #f #f #f)]
   [(char-whitespace? c)
    (values #f 'white-space #f position (+ 1 position))]
   [else
    (case c
      [(#\` #\~)
       (values #f 'parenthesis #f position (+ 1 position))]

      [(#\. #\?)
       (if (eof-object? (read-char port))
	   (values #f 'string #f position (+ 1 position))
	   (values #f 'string #f position (+ 2 position)))]

      [else
       (values c 'symbol #f position (+ 1 position))])]))

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
	 (if (or (eof-object? a) (eof-object? b))
	     eof
	     (decorate (list a b) (- tail-position position))))]

      [(#\~)
       (let ([namec (read-char port)]
	     [term (read-term src port)])
	 (define-values (l c tail-position) (port-next-location port))
	 (if (or (eof-object? namec) (eof-object? term))
	     eof
	     (decorate `(%define ,(string->symbol (string namec)) ,term)
		       (- tail-position position))))]

      [(#\.) (let ([c (read-char port)])
	       (if (eof-object? c)
		   eof
		   (decorate `(%dot ,c) 2)))]

      [(#\?) (let ([c (read-char port)])
	       (if (eof-object? c)
		   eof
		   (decorate `(%question ,c) 2)))]

      [else (decorate (string->symbol (string c)) 1)])]))
