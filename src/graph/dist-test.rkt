#lang racket

(require rackunit)

(require "dist.rkt")

(check-equal? (dif (vector) (vector)) (vector) "dif of vector with no size")
(check-equal? (dif (vector 1.) (vector 1.)) (vector 0.) "dif must be vector 0")
(check-equal? (dif (vector 1. 2.) (vector 1. 1.2)) (vector 0. 0.8) "dif of size 2 not equal")

(check-equal? (dist (vector 0. 3.) (vector 4. 0.)) 5.  "dif of pitagora theorem not equal")