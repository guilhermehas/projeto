#lang racket

(require rackunit)

(require "graph.rkt")

(check-equal? (dif (vector) (vector)) (vector) "dif of vector with no size")
(check-equal? (dif (vector 1.) (vector 1.)) (vector 0.) "dif must be vector 0")
(check-equal? (dif (vector 1. 2.) (vector 1. 1.2)) (vector 0. 0.8) "dif of size 2 not equal")

(check-equal? (dist (vector 0. 3.) (vector 4. 0.)) 5.  "dif of pitagora theorem not equal")

(check-equal? (enumerate-list (list) 0) (list) "enumerate empty list")
(check-equal? (enumerate-list (list 2) 0) (list (list 2 0)) "enumerate list of element 2")
(check-equal? (enumerate-list (list 2) 2) (list (list 2 2)) "enumerate list of element 2")

(check-equal? (scalar-prod (vector) (vector)) 0)
(check-equal? (scalar-prod (vector 2 3) (vector 100 1000)) 3200)

(check-equal? (cos-dist (vector 1) (vector 3)) -1)
(check-equal? (cos-dist (vector 1 0) (vector 3 0)) -1)
(check-equal? (cos-dist (vector 0 1) (vector 3 0)) 0)
(check-equal? (cos-dist (vector 2 0) (vector 3 4)) -3/5)


(let* (
    [law (vector 0)]
    [ans1 (list (vector 1) 3)]
    [ans2 (list (vector 2) 2)]

    [ans1withD (list 3 1.)]
    [ans2withD (list 2 2.)]

    [vans (list (vector 1) (vector 2))])

    (check-equal? (get-shortest-answer law (rest vans)) (list 0 2.))
    (check-equal? (get-shortest-answer law vans) (list 0 1.))
    (check-equal? (get-shortest-answer law (reverse vans)) (list 1 1.)))


(let* (
    [question (vector 0)]
    [question2 (vector 1)]
    [laws (list (vector -1.) (vector 2.) )]
    [answers (list (vector 0.) (vector 1.) )])

    (check-equal? (get-best-law question laws answers) (list 2. 0 0))
    (check-equal? (get-best-law question2 laws answers) (list 2. 1 1)))


(let* (
    [question (vector 0 1)]
    [question2 (vector 1 0)]
    [laws (list (vector 3 4) (vector 1 0) )]
    [answers (list (vector 1 0) (vector 0 1) )])

    (check-equal? (get-best-law-cos question laws answers) (list -8/5 0 1))
    (check-equal? (get-best-law-cos question2 laws answers) (list -2 1 0)))

