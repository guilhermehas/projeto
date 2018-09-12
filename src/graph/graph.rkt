#lang racket

 (require racket/trace)

(provide 
norm
dif
dist
enumerate-list
get-shortest-answer
get-best-law
scalar-prod
cos-dist
get-best-law-cos
)

;obs: Para calculo de distancia, foi usada distancia euclideana.
;calcula a norma de vetor.
(define (norm vec)
    (sqrt (for/fold ([sum 0.0])
        ([x (in-vector vec)])
        (+ sum (* x x)))))

;calcula a diferença entre cada elemento (v1-v2).
(define (dif v1 v2)
    (vector-map - v1 v2))

;define valor da distancia do vetor. 
(define (dist v1 v2)
    (norm (dif v1 v2)))

(define (scalar-prod v1 v2)
    (for/sum 
        ([i v1] [j v2])
        (* i j)))

(define (cos-dist v1 v2)
    (- 1 (/
        (scalar-prod v1 v2)
        (sqrt (*
            (scalar-prod v1 v1)
            (scalar-prod v2 v2))))))

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
(define-values (_ get-best-law-cos) (funcs-from cos-dist))