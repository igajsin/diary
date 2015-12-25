(define-module (euler euler6)
  #:use-module (euler util)
  #:use-module (srfi srfi-1))

(define (sum-of-squares lborder uborder)
  (fold (lambda (x acc) (+ acc (expt x 2)))
	0
	(oseq lborder uborder)))

(define (square-of-sums lborder uborder)
  (expt (fold + 0 (oseq lborder uborder)) 2))

(define (is-prime1 num k uborder)
  (cond
   ((> k uborder) #t)
   ((is-devisor? num k) #f)
   (#t (is-prime1 num (1+ k) uborder))))

(define (is-prime? num)
  (is-prime1 num 2 (sqrt num)))
