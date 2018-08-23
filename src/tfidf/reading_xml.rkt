#lang racket

(require xml)
(require txexpr)

; retorna o texto da primeira questão
; o "filter string?" é para retirar os caracteres inadequados, quando o XML original for refeito poderemos retirar esta parte

(define fp (open-input-file "2010-01.xml" #:mode 'text))
(define arq (xml->xexpr (document-element (read-xml fp))))
(define L (get-elements arq))
(define L-first-third-element (third (first L)))
(define rest-L (rest (rest L-first-third-element)))
(define just-L-string (filter string? rest-L))
(define text-string (string-join just-L-string))
text-string