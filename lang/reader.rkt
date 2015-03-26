#lang s-exp syntax/module-reader
unlambda/lang/language

#:read unlambda-read
#:read-syntax unlambda-read-syntax

(require "../parser.rkt"
	 ;; Parses to generate an AST. Identifiers in the AST
	 ;; are represented as syntax objects with source location.
	 )

(define (unlambda-read in)
  (syntax->datum (read-term #f in)))

(define (unlambda-read-syntax src in)
  (read-term src in))
