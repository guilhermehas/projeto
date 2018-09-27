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
    "parsers/read_law.rkt"
    "tfidf/tfidf.rkt"
    "graph/graph.rkt"
    "data-structures.rkt"
    )

  (define articles-path (make-parameter "data/raw/articles/"))
  (define exams-path (make-parameter "data/raw/exams/"))

  (define exam-path
    (command-line
     #:program "projeto-eda"
     #:usage-help
     "Solve OAB exams through tf-idf"
     "---------------------"
     #:once-each
     [("-l" "--articles-path") lawspath
                             "Setting path to dir where the laws are archived"
                             (articles-path lawspath)]
     [("-e" "--exams-path") exampath
                             "Setting path to dir where the laws are archived"
                             (exams-path exampath)]

     #:args (exam)

     (string-append (exams-path) exam)))

  ;(listof question) -> (listof (listof documents))
  (define (prepare-one-exam exam)
    (for/fold ([questions-answers null]
               #:result (reverse questions-answers))
              ([question exam])
      (cons (cons (document question)
                  (map document (question-items question)))
            questions-answers)))

  ;(listof article) -> (listof documents)
  (define (prepare-articles art)
    (map document art))

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


  (define (convert-output question-struct laws result)
    (define article (list-ref laws (second result)))
    (list (question-number question-struct)
          (third result)
          (article-law article)
          (article-art-number article)))

  (define (main articles-path exam-path)

    (displayln articles-path)
    (displayln exam-path)

    (define articles (read-law articles-path))
    (define exam (read-exam exam-path))

    (define list-questions (prepare-one-exam exam))
    (define list-articles (prepare-articles articles))

    (define output (list))
    (for ((question list-questions))
      (define-values (q i a) (apply-tfidf question list-articles))
      (define-values (min-dist best-art best-ans)
                    (get-distance-article-answer (first (map node q))
                                                  (map node a)
                                                  (map node i)))
      (set! output (cons (list (first question) min-dist best-art best-ans)
                   output)))

    (displayln output))

  (main (articles-path) exam-path))
