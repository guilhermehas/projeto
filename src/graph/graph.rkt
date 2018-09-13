#lang racket

 (require racket/trace)
 (require math/matrix)
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

(define (mul-list lst x)
  (map (lambda (n) (* x n)) lst))

(define (add-list lst1 lst2)
    (for/sum 
        ([i lst1] [j lst2])
        (+ i j)))

;norma-p, x>0
(define (p-sum x p)
(for/sum ([i x]) (expt (list-ref x (- i 1)) p)))
(define (p-norm x p)
  (expt (p-sum x p) (/ 1 p)))


;simulador cos(x,x0) = dot(x,x0)/((|x|^2)*(|x0|^2))
(define (simf1 v1 v2)
  (/ (scalar-prod v1 v2)
     (sqrt (*
            (scalar-prod v1 v1)
            (scalar-prod v2 v2)))
     )
  )
(define (simf2 v1 v2)
  (scalar-prod (- v1 v2) (- v1 v2)))
(define (simgradf2 v1 v2)
  (mul-list (add-list v1 (mul-list v2 (- 0 1))) 2))



;simulador gradiente da funcao f(x) := cos(x,x0) - aqui queremos x0 fixo para rodar o metodo do gradiente
(define (simgradf1 v1 v2)
  (mul-list v1 (/ (scalar-prod v1 (- v1 v2)) (* (expt (sqrt (scalar-prod v1 v1)) 3) (sqrt (scalar-prod v2 v2))))))


;note que toda norma é convexa ||tx+(1-t)y|| <= t||x|| + (1-t)||y||
;além disso algumas normas sao diferenciaveis longe dos eixos!
;ex: normas Lp

;podemos portanto formular, para uma dada lei x0, o problema como
;um problema de otimizacao: min ||x-x*||, x ∈ R^n, onde
;-x0 representa o vetor pergunta (iteracao 0)
;-x* representa a lei
;Podemos utilizar, por exemplo, o metodo do gradiente 
;e calcular quantas iteracoes precisamos para ir x0--->x*
;se considerarmos que uma resposta valida deve estar dentro
;de uma bola em torno de x*
;----note: x* é um minimo para a funcao em questao 


;metodo do gradiente, t = stepSize
(define (gradMethod x x0 tolerance t k)
  (cond
    ;note: é importante 
    [(< (norm (simgradf2 x x0)) tolerance) (x (simf2 x x0) k)]
    [else (gradMethod ((add-list x (mul-list (simgradf2 x x0) (- t 0)))) x0 tolerance t (+ k 1))]
    ))

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
(define-values (_ get-best-law-cos) (funcs-from dist))
