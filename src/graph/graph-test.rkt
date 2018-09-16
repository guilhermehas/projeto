#lang racket

(require rackunit)
(require racket/block)

(require "graph.rkt")

;;; (block
;;;     (define node1 (node "n1" (vector 0) (list)))
;;;     (define graph (list node1))
;;;     (define-values (distances previous) (dijkstra graph node1))
;;;     (check-equal? (dict-ref distances node1) 0))

(block
    (define node1 (node "n1" (vector 0) (list)))
    (define node2 (node "n2" (vector 1) (list node1)))
    (set-node-neineighbors! node1 (list node2))

    (define graph (list node1 node2))
    (define-values (distances previous) (dijkstra graph node1))
    (println distances)
    ;;; (check-equal? (dict-ref distances node1) 0)
    ;;; (check-equal? (dict-ref distances node2) 1)
    ;;; (check-equal? (dict-ref previous node2) node1))

)