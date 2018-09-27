## Solving OAB Exams!

### Try it yourself

`racket src/main.rkt 2010-01.xml`

It will print an output which the results are a list of question of the 
selected exam with the answers:

    -question (document struct)
    -min-dist (float)
    -best-article (document struct)
    -best-answer  (docuemnt struct)

### Installing

From root directory, install dependencies with

```
raco pkg install src
raco pkg install https://github.com/n3mo/data-science.git
```


if its package is already intalled, update the dependencies

`raco pkg update --link src`

### Graph

Struct that defines the node in graph
```racket
(struct node (document vector [neineighbors #:mutable #:auto])
    #:auto-value (list)
    #:transparent)
```

Function that returns Dijkstra algorithm from a distance function
```racket
(dij-from dist)
```

Transform into graph from question, answers and a list of intermediary layers of articles
```racket
(to-graph question answers . list-articles)
```

Calculates the shortest distance, the best article and the best answer of a graph with a question, an intermediate layer of articles and a final layer of answers
```racket
(get-distance-article-answer question articles answers)
```


### TF-IDF

```racket
(tf-idf corpus)
```

Calculates a document-term-matrix for the text in `corpus` (wherein matrix rows correspond to documents and columns to individual words/tokens). More specifically, the **term frequency-inverse document frequency** (tf-idf) matrix is returned. `corpus` should be two or more statments. The racket/math matrix is returned in a list along with a list of words/tokens found across all statments. The order of the list of tokens corresponds to the columns in the returned tf-idf.


```racket
;Just a simple corpus
(define text-e "guilherme foi Para a praia e foi\n para\t\n a fazenda. Eu não fui para praia nem para a fazenda")
(define text-f "joao foi para praia, rademacker para a fazenda mas não gostou")
(define corpus `("texto sem contexto" ,text-e ,text-f))

;Convert a list of strings in a tf-idf matrix
(tf-idf corpus)
;(list
; '("gostou" "texto" "fazenda" "contexto" "joao" "rademacker" ;"praia" "guilherme")
; (array
;  #[#[0 0.23856062735983122 0 0.23856062735983122 0 0 0 0]
;    #[0 0 0.0704365036222725 0 0 0 0.0704365036222725 0.09542425094393249]
;    #[0.09542425094393249 0 0.03521825181113625 0 0.09542425094393249 0.09542425094393249 0.03521825181113625 0]]))
```


### Coverage
To execute coverage command, run:
```bash
raco cover -f html src
```

### Participações
| Aluno         | Tarefas                                        |
| ------------- |:----------------------------------------------:| 
| Guilherme     | Grafos (dijkstra), testes, cobertura de testes |
| João          | Juntar partes do projeto                       |
| Hugo          | TF-IDF                                         |
| Pedro         | Cálculo de Distâncias                          |
| Alexandre     | Parser do documento                            |