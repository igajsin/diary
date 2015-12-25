(define-module (euler euler5)
  #:use-module (euler util)
  #:use-module (srfi srfi-1))

(define step (* 2 3 5 7 11 13 17 19))

(define init-step (fold (lambda (x acc) (* x acc)) 1 (oseq 2 20)))

(define (check num uborder)
  (fold (lambda (x acc)
	  (and acc (is-devisor? num x)))
	#t
	(oseq 2 uborder)))

(define (findd k step)
  (if (check k 20) k
      (findd (+ k step) step)))


