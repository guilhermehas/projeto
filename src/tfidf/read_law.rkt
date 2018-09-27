#lang racket/base

; calling (P) and (L) results in the 2010-01 test and the 8906 law, respectively.

(require
 racket/string
 racket/list
 txexpr
 xml
 xml/path)

(require (except-in "../data-structures.rkt" struct:document document document? document-statement document-type))

(provide
 read-law
;(struct-out article)
)

(define (lei-xml->xexpr fp)
  (with-input-from-file fp
    (lambda () (xml->xexpr (document-element (read-xml (current-input-port)))))
    #:mode 'text))

; (article integer? string? )
;(struct article (law art-number statement) #:transparent)
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


(define (article-list->article-struct list-of-articles law); law-name) it's possible to add "law name" to know from which law is eache article
  (let ((counter 0))
    (for/list ((a list-of-articles))
      (set! counter (add1 counter))
      (article law counter a)))); a way would be here, changing the struct of article to include law-name

;;; (define lei (lei->xexpr "data/raw/leis/lei-8906.xml"))

(define (read-law path)
    (define law-xml-files (map path->string (directory-list path)))
    (define laws '())
    (for ((law-file law-xml-files))
        (set! laws (append
                (article-list->article-struct
                    (lei->artigos (lei-xml->xexpr
                        (string-append path law-file)))
                        (first (string-split law-file ".xml")))
                laws)))
    laws)
