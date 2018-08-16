#lang racket

 (require racket/trace)

(provide 
norm
dif
dist
enumerate-list
update-new-answer
get-shortest-answer
update-law
get-shortest-law-answer
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

;constroi nova lista com valores enumerados.
(define (enumerate-list lista start)
    (define (aux lista i)
        (match lista
            [(list) (list)]
            [el (cons (list (first el) i) (aux (rest el) (add1 i)))]))
    (aux lista start))

;funçao que sera aplicada em get-shortest-answer ao usar foldl na lista de answers.
;obs: deveria ter opcao de msg de erro caso nao retorne a lista indicadaem match
(define (update-new-answer law last-min-dist-index answer-index)
    (match (list last-min-dist-index answer-index)
        [(list (list index dist-old) (list new-answer new-index))
        (let* (
            [dist-new (dist law new-answer)])
            (if (< dist-new dist-old)
                (list new-index dist-new)
                (list index dist-old)))]))

;cria lista de distancia entre lei e respostas
(define (get-shortest-answer law answers)
    (match answers
        [(list) (list -1 0.)]
        [(list a) (list 0 (dist law a))]
        [ans
        (let*(
            [f (first ans)]
            [r (rest ans)]
            [d0 (dist law f)])
            (foldl (lambda (x y) (update-new-answer law y x)) (list 0 d0) (enumerate-list r 1)))]))

(define (update-law answers old-lawi-ansi-dist new-law-lawi)
    (match (list old-lawi-ansi-dist new-law-lawi)
        [(list 
            (list lawi ansi old-dist)
            (list new-law new-lawi))
                (let* (
                    [new-ansi-dist (get-shortest-answer new-law answers)])
                    (match new-ansi-dist
                        [(list new-ansi new-dist)
                            (if (< new-dist old-dist)
                                (list new-lawi new-ansi new-dist)
                                old-lawi-ansi-dist)]))]))

(define (get-shortest-law-answer laws answers)
    (define first-term (list -1 -1 +inf.f))
    (define (up-law law term)
        (update-law answers term law))
    (foldl up-law first-term (enumerate-list laws 0)))