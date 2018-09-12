#lang racket

(require data-science)

(provide tf-idf)

;;; (define text-e "guilherme foi Para a praia e foi\n para\t\n a fazenda. Eu não fui para praia nem para a fazenda")
;;; (define text-f "joao foi para praia, rademacker para a fazenda mas não gostou")
;;; (define corpus `("texto sem contexto" ,text-e ,text-f))


;; Set[String]
(define stopwords
        (list->set
            (map (lambda (x) (string-trim x))
                (file->lines "tfidf/stopwords.txt"))))


(define (treat-strings string)
    (filter-not (λ (e) (set-member? stopwords e))
    ((compose string-split remove-punctuation string-downcase string-normalize-spaces) string)))
                         

;; string --> (list of (list of 'str' float))
(define (string->tokens string)
    (let* ((list-string (treat-strings string))
            (total (length list-string)))
                (map (lambda (x) (list (first x) (length x)))
                (group-by (lambda (x) x) list-string))))

;;; (list of strings) --> list of (list of strings) and array of floats
(define (tf-idf corpus)
    (apply dtm (map string->tokens corpus)))

