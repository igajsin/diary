(define-module (euler euler9a)
  #:use-module (euler util)
  #:use-module (srfi srfi-1))

(define (is-triplet? p? triplet)
  (if (eq? triplet '()) #f
      (let ((a (list-ref triplet 0))
	    (b (list-ref triplet 1))
	    (c (list-ref triplet 2)))
	(p? a b c))))

(define (is-simple-triplet? triplet)
  (is-triplet?
   (lambda (a b c) (and (> b a)
		   (> c b)
		   (= (expt c 2)
		      (+ (expt a 2) (expt b 2)))))
   triplet))

(define (is-correct-triplet? triplet)
  (is-triplet?
   (lambda (a b c)
     (and (is-simple-triplet? triplet)
	  (= 1000 (+ a b c))))
   triplet))

(define (find-one good? end? proc n)
  (if (end? n) '()
      (let ((pn (proc n)))
	(if (good? pn) pn
	    (find-one good? end? proc (1+ n))))))

(define (find-triplet)
  (find-one is-correct-triplet?
	    (lambda (x) (> x 1000))
	    (lambda (x) (list 200 375 x))
	    0))

(define (find-triplet)
  (find-one is-correct-triplet?
	    (lambda (x) (> x 1000))
	    (lambda (x)
	      (find-one is-correct-triplet?
			(lambda (y) (= y x))
			(lambda (y) (list 200 y x))
			1))
	    1))

(define (find-triplet)
  (find-one is-correct-triplet?
	    (lambda (x) (> x 1000))
	    (lambda (x)
	      (find-one is-correct-triplet?
			(lambda (y) (= y x))
			(lambda (y)
			  (find-one is-correct-triplet?
				    (lambda (z) (= z y))
				    (lambda (z) (list z y x))
				    1))
			1))
	    1))



