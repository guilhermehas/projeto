#lang racket/base

; calling (P) and (L) results in the 2010-01 test and the 8906 law, respectively.

(require
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

(define exam (read-exam "data/raw/provas/2010-01.xml"))

(define laws (read-law "data/raw/leis/"))

(define (prepare-one-exam exam)
  (define questions-answers '())
  (for ((question exam))
    (set! questions-answers (cons (cons (question-statement question)
                                    (map (lambda (x) (item-statement x)) 
                                      (question-items question)))
                                   questions-answers))
  )
  (reverse questions-answers)
)

(define (prepare-laws laws)
  (map (lambda (x) (article-statement x)) laws)
)

(define (array->listofvectors array)
  (map list->vector (array->list* array))
)

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
  (values question-vector answers-vector laws-vector)
)


(define list-questions (prepare-one-exam exam))
(define list-laws (prepare-laws laws))

(define (apply-model tfidf-func question list-laws)
  (let-values (((question answers laws) (tfidf-func question list-laws)))
    (get-best-law question laws answers))
)


;;; (apply-tfidf (first list-questions) list-laws)
(apply-model apply-tfidf (second list-questions) list-laws)

