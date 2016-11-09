(define-module (euler euler12)
  #:use-module (euler util)
  #:use-module (srfi srfi-1))

(define (triangle-number number)
  (if (= number 1) 1
      (+ number (triangle-number (1- number)))))

(define* (number-from-triangle triangle #:optional (n 1))
  (let ((k (- triangle n)))
    (if (= k 0) n
	(number-from-triangle k (1+ n)))))

(define* (count-devisors n #:optional k (acc 1))
  (let ((k1 (or k (quotient n 2))))
    (if (< k1 1) acc
	(count-devisors n (1- k1)
			(if (is-devisor? n k1) (1+ acc) acc)))))

(define* (list-of-devisors n #:optional k (acc '()))
  (let ((k1 (or k n)))
    (cond
     ((<= k1 0) acc)
     ((is-devisor? n k1) (list-of-devisors n (1- k1) (cons k1 acc)))
     (#t (list-of-devisors n (1- k1) acc)))))

(define* (find-triangle-number max #:optional (current-number 1))
  (let* ((current-triangle-number (triangle-number current-number))
	(devisors (count-devisors current-triangle-number)))
    (if (> devisors max) current-triangle-number
	(find-triangle-number max (1+ current-number)))))

(define devisors (make-hash-table))

(define (check-devisor n d)
  (let ((ds (hash-ref devisors n (list n))))
    (or (member d ds)
	(if (is-devisor? n d) (hash-set! devisors n (cons d ds))
	    #f))))

(define* (find-devisors num #:optional (cur 1))
  (cons num
	(cond ((> cur (quotient num 2)) (list num))
	      ((check-devisor num cur)
	       (list cur (find-devisors (/ num cur) 2)))
	      (#t (find-devisors num (1+ cur))))))

(define primes '(2))

(define (primes? num)
  (cond
   ((member num primes) #t)
   ((is-prime? num) (set! primes (cons num primes)))
   (#t #f)))

(define (fill-primes num)
  (let ((end (find-border num))
	(start (car primes)))
    (do ((i start (1+ i)))
	((> i end))
      (primes? i))))

(define (find-border num)
  (let* ((rprimes (reverse (cons 1 primes)))
	 (first-devisor (car
			 (filter (lambda (x) (is-devisor? num x)) rprimes))))
    (/ num first-devisor)))

(define (fnd-dvsrs num)
  (fill-primes num)
  (map (lambda (prime)
	 (if (is-devisor? num prime)
	   (cons prime
		 (fnd-dvsrs (/ num prime)))
	   '()))
       (or (member (find-border num) primes >=) '())))

(define* (members x lst #:optional (= equal?))
  (or (member x lst =)
      '()))

(define (fill-primes num)
  (let ((end (find-border num))
	(start (car primes)))
    (do ((i start (1+ i)))
	((> i end))
      (primes? i))
    (members end primes >=)))

(define (find-devs num)
  (let ((correct-primes (fill-primes num)))
    (map (lambda (y) (cons y (find-devs (/ num y))))
	 (filter (lambda (prime) (is-devisor? num prime)) correct-primes))))

(define (find-devisors n) 
  (filter (lambda (x) (is-devisor? n x)) (oseq 1 n)))

(define* (mul-three three #:optional (factor 1))
  (cond
   ((and (list? three)
	 (= 1 (length three)))
    (* factor (car three)))
   ((and (list? three)
	 (not (list? (car three))))
    (map (lambda (sub-three)
	   (list (car three)
		 (mul-three sub-three (car three))))
	 (cdr three)))
   ((and (list? three)
	 (list? (car three)))
    (map mul-three three))
   (else (format #f "malformed three ~a" three))))

(define ex '(5 (3 (2)) (2 (3) (2))))


(define* (wlk three #:optional (acc '()))
  (if (null? three) acc
      (let ((entry (car three))
	    (leafs (cdr three)))
	(map (lambda (sub-three) (wlk sub-three (cons entry acc)))
	     leafs))))

(define (wlk2 three)
  (reduce (lambda (sub-three acc)
	    (if (null? sub-three) '()
		(cons acc (wlk2 sub-three))))
	  '()
	  three))

(define (wlk3 tree)
  (let ((hd (car tree))
	(leafs (cdr tree)))
    (map (lambda (leaf) (* hd (car leaf))) leafs)))
