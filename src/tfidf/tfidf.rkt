#lang racket

(define (treat-strings string)
    (string-downcase (string-normalize-spaces string)))

;; string --> (list of (list of 'str' float))
(define (tf string)
    (let* ((list-string (string-split (treat-strings string)))
            (total (length list-string)))
        (map (lambda (x) (list (first x) (/ (second x) total)))
            (map (lambda (x) (list (first x) (length x)))
            (group-by (lambda (x) x) list-string))))
)

(tf "guilherme foi Para a praia e foi para a fazenda")