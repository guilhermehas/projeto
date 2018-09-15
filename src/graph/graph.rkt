#lang racket

(require racket/trace)
(require math/matrix)
(require "dist.rkt")

(provide
    enumerate-list
    get-shortest-answer
    get-best-law
)

;constroi nova lista com valores enumerados.
(define (enumerate-list lista start)
    (define (aux lista i)
        (match lista
            [(list) (list)]
            [el (cons (list (first el) i) (aux (rest el) (add1 i)))]))
    (aux lista start))

(define (funcs-from dist)
    ;cria lista de distancia entre lei e respostas
    (define (get-shortest-answer law questions)
        (define (get-distance-law-question question-enum)
            (match-define (list question i) question-enum)
            (dist question law))
        (define questions-enum (enumerate-list questions 0))
        (define question-enum (argmin get-distance-law-question questions-enum))
        (match-define (list question i) question-enum)
        (define dist-question-answer (dist question law))
        (list i dist-question-answer))


    ;cria lista de distancia entre lei e respostas
    (define (get-best-law question laws answers)
        (define laws-enum (enumerate-list laws 0))

        (define (update-law-dist-lawid-answerid law-id)
            (match-define (list law idlaw) law-id)
            (match-define (list id-answer dist-law-answer) (get-shortest-answer law answers))
            (define law-dist (dist question law))
            (list (+ law-dist dist-law-answer) idlaw id-answer))

        (define updated-laws (map update-law-dist-lawid-answerid laws-enum))
        (argmin (lambda (x) (first x)) updated-laws))

    (values get-shortest-answer get-best-law))

(define-values (get-shortest-answer get-best-law) (funcs-from dist))