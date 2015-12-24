(define-module (euler euler5)
  #:use-module (euler util)
  #:use-module (srfi srfi-1))

(define step (* 2 3 5 7 11 13 17 19))

(define (is-res? n)
  (fold (lambda (x acc) (and acc (is-devisor? n x)))
	#t (oseq 2 20)))

(define (down-wlk num step)
  (if (> step num) #f
      (if (is-res? num) num
	  (down-wlk (- num step) step))))
