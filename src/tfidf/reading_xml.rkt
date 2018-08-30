#lang racket

(require
 racket/string
 racket/list
 txexpr
 xml
 xml/path
 )

; retorna o texto da primeira questão
; o "filter string?" é para retirar os caracteres inadequados, quando o XML original for refeito poderemos retirar esta parte

(define (get-sentence)
    (string-join (filter string? (rest (rest
                (third (let* ((fp (open-input-file "data/raw/provas/2010-01.xml" #:mode 'text))
                              (arq (xml->xexpr (document-element (read-xml fp))))
                              (L (get-elements arq)))
                         (first L))))))))

(define (prova->xexpr fp)
  (with-input-from-file fp
    (lambda () (xml->xexpr (document-element (read-xml (current-input-port)))))
    #:mode 'text))

; (question integer? boolean? string? string? item?)
(struct question (number answer area statement items) #:transparent)

; (item symbol? string?)
(struct item (letter statement) #:transparent)

;; homework: use for/fold to get items, statement and correct in one go
; xexpr -> (listof question?)
(define (xexpr->exam xe)
  (define (get-letter item)
    (string->symbol (string-upcase (attr-ref item 'letter))))
  (define (get-answer xelems)
    (for/or ([elem (in-list xelems)])
      (and (txexpr? elem)
           (equal? (attr-ref elem 'correct "false") ; only items have correct attr
                   "true")
           (get-letter elem))))
  (define (get-statement xelems)
    (for/or ([elem (in-list xelems)])
      (and (txexpr? elem)
           (eq? (get-tag elem) 'statement)
           (string-join (get-elements elem)))))
  ; (listof ) -> (listof item?)
  (define (get-items xelems)
    (for/list ([elem (in-list xelems)]
               #:when (and (txexpr? elem) (eq? (get-tag elem) 'item)))
      (item (get-letter elem)
            (string-join (get-elements elem)))))
  ;;
  (for/list ([xeq (in-list (get-elements xe))]
             #:unless (string? xeq))
    (let ([xelems (get-elements xeq)])
      (question (attr-ref xeq 'number)
                (if (attr-ref xeq 'valid)
                    (get-answer xelems)
                    #f)
                (attr-ref xeq 'area)
                (get-statement xelems)
                (get-items xelems)))))

(define prova (prova->xexpr "data/raw/provas/2010-01.xml"))

(xexpr->exam prova)
