#lang racket/base

; calling (P) and (L) results in the 2010-01 test and the 8906 law, respectively.
(module+ main
  (require
    racket/cmdline
    racket/string
    racket/list
    txexpr
    math/array
    "parsers/read_exam.rkt"
    "parsers/read_articles.rkt"
    "tfidf/tfidf.rkt"
    "graph/graph.rkt"
    "data-structures.rkt"
    )

  (define articles-path (make-parameter "data/raw/articles/"))
  (define exams-path (make-parameter "data/raw/exams/"))
  (define output-type (make-parameter "simple"))

  (define exam-path
    (command-line
     #:program "projeto-eda"
     #:usage-help
     "Solve OAB exams through tf-idf"
     "---------------------"
     #:once-each
     [("-a" "--articles-path") lawspath
                               "Setting path to dir where the laws are archived"
                               (articles-path lawspath)]
     [("-e" "--exams-path") exampath
                            "Setting path to dir where the laws are archived"
                            (exams-path exampath)]
     [("-o" "--output-type") outype
                            "Set the type of output: simple or complete"
                            (output-type outype)]

     #:args (exam)

     (string-append (exams-path) exam)))

  ;(listof question?) -> (listof (listof documents?))
  (define (prepare-one-exam exam)
    (for/fold ([questions-answers null]
               #:result (reverse questions-answers))
              ([question exam])
      (cons (cons (document question)
                  (map document (question-items question)))
            questions-answers)))

  ;(listof (listof article?) -> (listof documents?)
  (define (prepare-articles art)
    (map document (flatten art)))

  ;(listof documents) and (listof documents) -> (listof documents), (listof documents) and (listof documents)
  (define (apply-tfidf question-item-docs laws-docs)
    (define updated-docs (second (tf-idf (append question-item-docs laws-docs))))
    (for/fold ([question null]
               [items null]
               [laws null]
               #:result (values (reverse question) (reverse items) (reverse laws)))
              ([doc (in-list updated-docs)])
      (cond [(eq? (document-type doc) 'question) (values (cons doc question) items laws)]
            [(eq? (document-type doc) 'item) (values question (cons doc items) laws)]
            [(eq? (document-type doc) 'article) (values question items (cons doc laws))])))

  (define (apply-model list-question-item-docs laws-docs)
    (for/fold ([output null]
              #:result (reverse output))
              ([question-item-docs list-question-item-docs])
      (define-values (q i a) (apply-tfidf question-item-docs laws-docs))
      (define-values (min-dist best-art best-ans)
                      (get-distance-article-answer (first (map node q))
                                                   (map node a)
                                                   (map node i)))
                  ; Document,                      Float, Node , Node
      (define question-doc (first question-item-docs))
      (define-values (correct-answer model-predicted-answer)
        (values (question-answer (document-source question-doc)) (item-letter (document-source (node-document best-ans)))))
      (cons (list question-doc min-dist (node-document best-art) (node-document best-ans) correct-answer (eq? correct-answer model-predicted-answer))
            output)))


  (define (simple-output output)
    (append
      (list
        (question-number (document-source (car output)))
        (cadr output)
        (document-source (caddr output))
        (item-letter (document-source (cadddr output))))
      (cddddr output)))


  (define (convert-output output-list output-type)
    (cond [(eq? output-type "simple") (displayln "simple")
              (for ((output output-list))
                (displayln (simple-output output)))]
          [else (displayln "complete")
            (for ((output output-list))
              (displayln output))]))

  (define (main articles-path exam-path output-type)

    (let ([list-questions (prepare-one-exam (read-exam exam-path))]
          [list-articles (prepare-articles (read-articles articles-path))])
      (convert-output (apply-model list-questions list-articles)
                      output-type)))

  (main (articles-path) exam-path (output-type)))
