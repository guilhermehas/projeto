#lang racket/base

; calling (P) and (L) results in the 2010-01 test and the 8906 law, respectively.
(module+ main
  (require
    racket/cmdline
    racket/string
    racket/list
    txexpr
    math/array
    "tfidf/read_exam.rkt"
    "tfidf/read_law.rkt"
    "tfidf/tfidf.rkt"
    "graph/graph.rkt"
    "data-structures.rkt"
    )

  (define laws-path (make-parameter "data/raw/leis/"))
  (define exams-path (make-parameter "data/raw/provas/"))

  (define cmd-line
    (command-line
     #:program "projeto-eda"
     #:usage-help
     "Solve OAB exams through tf-idf"
     "---------------------"
     #:once-each
     [("-l" "--laws-path") lawspath
                             "Setting path to dir where the laws are archived"
                             (laws-path lawspath)]
     [("-e" "--exams-path") exampath
                             "Setting path to dir where the laws are archived"
                             (exams-path exampath)]

     #:args (exam)

     (string-append (exams-path) exam)))

  ;----------------------------------------------------------
  ;old definition
  #;(define (prepare-one-exam exam)
    (define questions-answers '())
    (for ((question exam))
      (set! questions-answers (cons (cons (question-statement question)
                                          (map (lambda (x) (item-statement x))
                                               (question-items question)))
                                    questions-answers)))
    (reverse questions-answers))

  #;(define (prepare-laws laws)
    (map (lambda (x) (article-statement x)) laws))

  #;(define (array->listofvectors array)
    (map list->vector (array->list* array)))


  #;(define (apply-tfidf question list-laws)
    (define tfidf-matrix (second (tf-idf (append question list-laws))))
    (define question-vector (array->vector (array-slice-ref tfidf-matrix
                                                            (list (list 0) (::)))))
    (define answers-vector
      (array->listofvectors
       (array-slice-ref tfidf-matrix (list (:: 1 5 1) (::)))))
    (define laws-vector
      (array->listofvectors
       (array-slice-ref tfidf-matrix (list (:: 6 #f 1) (::)))))
    (values question-vector answers-vector laws-vector))
  ;----------------------------------------------------------
  
  ;new definition
  
  ;(listof question) -> (listof (listof documents))
  (define (prepare-one-exam exam)
    (for/fold ([questions-answers null]
               #:result (reverse questions-answers))
              ([question exam])      
      (cons (cons (document question)
                  (map document (question-items question)))
            questions-answers)))

  ;(listof article) -> (listof documents)
  (define (prepare-laws laws)
    (map document laws))

  ;(listof documents) and (listof documents) -> (listof documents), (listof documents) and (listof documents)
  (define (apply-tfidf question-item-docs laws-docs)
    (define updated-docs (second (tf-idf (append question list-laws))))
    (for/fold ([question null]
               [items null]
               [laws null]
               #:result (values (reverse question) (reverse items) (reverse laws)))
              ([doc (in-list updated-docs)])
      (cond [(eq? (document-type doc) 'question) (values (cons doc question) items laws)]
            [(eq? (document-type doc) 'item) (values question (cons doc items) laws)]
            [(eq? (document-type doc) 'article) (values question items (cons doc laws))])))
    
  
  ;------------------------------------------------------------------
  ;legado, precisa ser modificado para funcionar na nova definição
  
  (define (apply-model tfidf-func question list-laws)
    (let-values (((question answers laws) (tfidf-func question list-laws)))
      (get-best-law question laws answers)))

  (define (convert-output question-struct laws result)
    (define article (list-ref laws (second result)))
    (list (question-number question-struct)
          (third result)
          (article-law article)
          (article-art-number article)))

  (define (main laws-path cmd-line)

    (define laws (read-law laws-path))
    (define exam (read-exam cmd-line))

    (define list-questions (prepare-one-exam exam))
    (define list-laws (prepare-laws laws))

    (define output (list))
    (for ((question list-questions)
          (question-exam exam))
       (displayln (question-number question-exam))
       (set! output
             (cons (convert-output question-exam laws
              (apply-model apply-tfidf question list-laws)) output)))
    output
    )

  (main laws-path cmd-line))
