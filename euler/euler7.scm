(define-module (euler euler7)
  #:use-module (euler util)
  #:use-module (srfi srfi-1))

(define (is-prime1 num k uborder)
  (cond
   ((> k uborder) #t)
   ((is-devisor? num k) #f)
   (#t (is-prime1 num (1+ k) uborder))))

(define (is-prime? num)
  (is-prime1 num 2 (sqrt num)))

(define (find-n-primes1 nlim primes n)
  (let* ((n? (is-prime? n))
	 (primes+ (if n? (cons n primes) primes)))
    (if (= nlim (length primes+)) primes+
	(find-n-primes1 nlim primes+ (1+ n)))))

(define (find-n-primes nlim)
  (find-n-primes1 nlim '() 2))
