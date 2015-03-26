#lang racket/base

(provide read-term)

(define (read-term src [port (current-input-port)])
  (define c (read-char port))
  (cond
   [(eof-object? c) eof]
   [(char-whitespace? c) (read-term src port)]
   [else
    (datum->syntax
     #f
     (case c
       [(#\`) `(,(read-term src port) ,(read-term src port))]
       [(#\.) `(%dot ,(read-char port))]
       [(#\?) `(%question ,(read-char port))]
       [(#\@) 'at]
       [(#\|) 'pipe]
       [(#\s) 's]
       [(#\k) 'k]
       [(#\i) 'i]
       [(#\d) 'd]
       [(#\r) `(%dot #\newline)]
       [(#\c) 'c]
       [(#\e) 'e]
       [(#\v) 'v]
       [else (error "parse error on" c)]))]))
