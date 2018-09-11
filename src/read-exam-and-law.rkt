#lang racket/base

; calling (P) and (L) results in the 2010-01 test and the 8906 law, respectively.

(require
 racket/string
 racket/list
 txexpr
 xml
 xml/path
 )

(define (prova->xexpr fp)
  (with-input-from-file fp
    (lambda () (xml->xexpr (document-element (read-xml (current-input-port)))))
    #:mode 'text))

(define prova (prova->xexpr "2010-01.xml"))

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


(define (P)
  (xexpr->exam prova))




(define (lei->xexpr fp)
  (with-input-from-file fp
    (lambda () (xml->xexpr (document-element (read-xml (current-input-port)))))
    #:mode 'text))

(define lei (lei->xexpr "lei-8906.xml"))

; (article integer? string? )
(struct article (art-number statement) #:transparent)
;obs: norma > titulo > capitulo > artigo > inciso, paragrafo > alinea, item


; txexpr? -> string?
(define (xexpr->string tx)
  (cond
    [(txexpr? tx)
     (string-join (map xexpr->string (get-elements tx)))]
    [(string? tx)
     tx]
    [else ""]))

; xexpr? -> (listof string?)
(define (lei->artigos tx)
  ; xexpr? -> boolean?
  (define (artigo? tx)
    (and (txexpr? tx) (eq? (get-tag tx) 'artigo)))
  ; xexpr? -> string?
  (define (gather-artigo art)
    (xexpr->string art))
  ;;
  (cond
    [(artigo? tx)
     (list (gather-artigo tx))]
    [(txexpr? tx)
     (append-map lei->artigos (get-elements tx))]
    [else null]))


(define (article-list->article-struct list-of-articles); law-name) it's possible to add "law name" to know from which law is eache article
  (let ((counter 0))
    (for/list ((a list-of-articles))
      (set! counter (add1 counter))
      (article counter a)))); a way would be here, changing the struct of article to include law-name

(define (L)
  (article-list->article-struct (lei->artigos lei)))
