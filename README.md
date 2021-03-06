## Solving OAB Exams!

### Try it yourself

`racket src/main.rkt 2010-01.xml`

For a quick run, try

`racket src/main.rkt -a data/raw/articles-test/ teste.xml`

It will print an output which is a list of questions of the selected exam with the answers:

`-o simple [default]`

A list containing

    -question (string)
    -min-dist (number)
    -best-article (string)
    -best-answer  (symbol)
    -correct-answer (symbol)
    -model-correct? (boolean)

`-o struct-simple`

The model-result-simple struct containing

    -question (number)
    -min-dist (number)
    -best-law (string)
    -best-art (number)
    -best-answer  (symbol)
    -correct-answer (symbol)
    -correct? (boolean)

`-o complete`

The model-result struct containing

    -question (document)
    -min-dist (number)
    -best-article (document)
    -best-answer  (docuemnt)
    -correct-answer (symbol)
    -correct? (boolean)


#### Distances
`-d --distance-function`

You may also change the distance function with the short command `-d` or its longer form `--distance-function`. Currently, there are Euclidian Distance `dist` [default] and Cosine Similarities `cos-dist`.

#### Differente Files and File Pahts

You can just add more exams at `data/raw/exams/` and call
them at `racket src/main.rkt \<your-exam>`.

All the articles are saved at `data/raw/articles/`.

If you need to change this path, you can pass a modifier

`-a --articles-path` to change the articles path
`-e --exams-path` to change the exams path

### Installing

From root directory, install dependencies with

```
raco pkg install https://github.com/n3mo/data-science.git
raco pkg install while-loop
raco pkg install txexpr
raco pkg install src/
```


if its packages are already intalled, update the dependencies

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

Calculates **tf-idf vector** for each `Document` in `corpus` and returns:

1. a list of words/tokens found across all statments corresponding to each dimension on the tf-idf vector Space (The order of the list of tokens corresponds to the columns in the returned tf-idf)
2. a list of Document wherin each `Document`'s `rep` field points to the respective **tf-idf vector**.

`corpus` should be a list of two or more `Documents`.


```racket
;Just a simple corpus
> (define i1 (item 'a "string item 1"))
> (define i2 (item 'b "string item 2"))
> (define doc-item1 (document i1))
> (define doc-item2 (document i2))
> (define doc-qt (document (question 1 'a "ethics" "string question 1" (list i1 i2))))
> (define doc-art (document (article "lei8096" 1 "string article 1")))
> (define corpus (list doc-qt doc-item1 doc-item2 doc-art))

;Convert a list of strings in a tf-idf matrix
> (tf-idf corpus)
(list
 '("article" "string" "item" "question")
 (list
  (document (question 1 'a "ethics" "string question 1" (list (item 'a "string item 1" 1) (item 'b "string item 2" 1))) '#(0 0 0 0.30102999566398114))
  (document (item 'a "string item 1" 1) '#(0 0 0.15051499783199057 0))
  (document (item 'b "string item 2" 1) '#(0 0 0.15051499783199057 0))
  (document (article "lei8096" 1 "string article 1") '#(0.30102999566398114 0 0 0))))
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
| Hugo          | TF-IDF, Data-Structures e Revisão              |
| Pedro         | Cálculo de Distâncias                          |
| Alexandre     | Parser do documento                            |
