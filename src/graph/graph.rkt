#lang racket

(require racket/block)
(require racket/trace)
(require racket/undefined)
(require data/heap)
(require dyoo-while-loop)

(require "dist.rkt")

(provide
    node
    set-node-neineighbors!
    dijkstra
)

(struct node (document vector [neineighbors #:mutable]) #:transparent)

(define (dij-from dist)
    (define (Dijkstra graph source)
        (define (operator-less a b)
            (< (cdr a) (cdr b)))
        (define node-queue (make-heap operator-less))
        (heap-add! node-queue (cons source 0))

        (define distances-from-source (make-hash))
        (for ([node graph])
            (dict-set! distances-from-source node +inf.f))

        (dict-set! distances-from-source source 0)
        (define previous (make-hash))

        (while (not (zero? (heap-count node-queue)))
            (match-define (cons u u-dist) (heap-min node-queue))
            (define u-vector (node-vector u))
            (define u-neigs (node-neineighbors u))
            (heap-remove-min! node-queue)
            (for ([v u-neigs])
                (define v-vector (node-vector v))
                (define alt (+ u-dist (dist u-vector v-vector)))
                (if (< alt (dict-ref distances-from-source v))
                    (block
                        (dict-set! distances-from-source v alt)
                        (dict-set! previous v u)
                        (heap-add! node-queue (cons v alt)))
                    void)))
        
        (values distances-from-source previous))

    Dijkstra)

(define dijkstra (dij-from dist))
