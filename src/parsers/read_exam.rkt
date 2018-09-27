#lang racket/base

(require
  racket/string
  racket/list
  txexpr
  xml
  xml/path)
 
(require (except-in "../data-structures.rkt" struct:document document document? document-statement document-type))

(provide
 read-exam
 )

(define (prova-xml->xexpr fp)
  (with-input-from-file fp
    (lambda () (xml->xexpr (document-element (read-xml (current-input-port)))))
    #:mode 'text))

; (question integer? boolean? string? string? item?)
;(struct question (number answer area statement items) #:transparent)

; (item symbol? string?)
;(struct item (letter statement) #:transparent)

; xexpr -> (listof question?)
(define (xexpr->questions xe)
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
  (for/list ([xeq (in-list (get-elements xe))]
             #:unless (string? xeq))
    (let ([xelems (get-elements xeq)]
          [num (string->number (attr-ref xeq 'number))])
      (define items (get-items xelems))
      (map (lambda (item) (set-item-question-number! item num)) items)
      (question num
                (if (attr-ref xeq 'valid)
                    (get-answer xelems)
                    #f)
                (attr-ref xeq 'area)
                (get-statement xelems)
                items))))

(define (read-exam path)
  (xexpr->questions (prova-xml->xexpr path)))
