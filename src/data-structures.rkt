#lang racket/base
;(module+ main

(require
  racket/string
  racket/list)

(provide
 (struct-out question)
 (struct-out item)
 (struct-out article)
 (struct-out document))
 

; (question integer? boolean? string? string? item?)
(struct question (number answer area statement items) #:transparent)

; (item symbol? string?)
(struct item (letter statement) #:transparent)

; (article integer? string? )
(struct article (law art-number statement) #:transparent)
;obs: norma > titulo > capitulo > artigo > inciso, paragrafo > alinea, item

; (document (or question? item? article?) )
(struct document (source [rep #:auto #:mutable])
  #:auto-value  null
  #:transparent)

(define (document-statement doc)
  (let ([source (document-source doc)])
    (cond [(item? source) (item-statement source)]
          [(question? source) (question-statement source)]
          [(article? source) (article-statement source)])))

(define

(define i1 (item 'a "string item 1"))
(define i2 (item 'b "string item 2"))
(define q1 (question 1 'a "ethics" "string question 1" (list i1 i2)))
(define a1 (article "lei8096" 1 "string article 1"))
(define doc-item (document i1))
(define doc-qt (document q1))
(define doc-art (document a1))

;)
