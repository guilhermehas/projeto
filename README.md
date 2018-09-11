## Start Coding

From root directory, install dependencies with

```
raco pkg install src
raco pkg install https://github.com/n3mo/data-science.git
```


if its package is already intalled, update the dependencies

`raco pkg update --link src`

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
