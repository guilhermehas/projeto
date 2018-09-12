#lang racket/base

; calling (P) and (L) results in the 2010-01 test and the 8906 law, respectively.
(module+ main
  (require
    racket/cmdline
    racket/string
    racket/list
    txexpr
    math/array
    xml
    xml/path
    "tfidf/read_exam.rkt"
    "tfidf/read_law.rkt"
    "tfidf/tfidf.rkt"
    "graph/graph.rkt"
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

  (define laws (read-law laws-path))
  (define exam (read-exam cmd-line))

  (define (prepare-one-exam exam)
    (define questions-answers '())
    (for ((question exam))
      (set! questions-answers (cons (cons (question-statement question)
                                          (map (lambda (x) (item-statement x)) 
                                               (question-items question)))
                                    questions-answers)))
    (reverse questions-answers))

  (define (prepare-laws laws)
    (map (lambda (x) (article-statement x)) laws))
    

  (define (array->listofvectors array)
    (map list->vector (array->list* array)))
    

  (define (apply-tfidf question list-laws)
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
    


  (define list-questions (prepare-one-exam exam))
  (define list-laws (prepare-laws laws))

  (define (apply-model tfidf-func question list-laws)
    (let-values (((question answers laws) (tfidf-func question list-laws)))
      (get-best-law question laws answers)))
    


  ;;; (apply-tfidf (first list-questions) list-laws)
  (apply-model apply-tfidf (second list-questions) list-laws))

  


  