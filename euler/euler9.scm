(define-module (euler euler9)
  #:use-module (euler util)
  #:use-module (srfi srfi-1))

(define (is-triplet? triplet)
  (let ((a (list-ref triplet 0))
	(b (list-ref triplet 1))
	(c (list-ref triplet 2)))
    (= (expt c 2)
       (+ (expt a 2) (expt b 2)))))

(define (is-correct-triplet? triplet)
  (let ((a (list-ref triplet 0))
	(b (list-ref triplet 1))
	(c (list-ref triplet 2)))
    (and (is-triplet? triplet)
	 (= 1000 (+ a b c)))))

(define (mkl2 p? uborder)
  (let ((triplets '()))
    (for-each
     (lambda (c)
       (for-each (lambda (b) (for-each
			 (lambda (a)
			   (let ((triple (list a b c)))
			     (when (p? triple)
			       (set! triplets (cons triple triplets)))))
			 (oseq 1 (1- b))))
		 (oseq 2 (1- c))))
     (oseq 3 uborder))
    triplets))


(define triplet '(200 375 425))
(define result 31875000)

