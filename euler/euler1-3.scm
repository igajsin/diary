;; igor.gajsin
;; wolspenopol

(use-modules (srfi srfi-1))

(define (seq n)
  (if (= n 0) '()
      (cons n (seq (- n 1)))))

(define (divisibility n)
  (or (= 0 (modulo n 3))
       (= 0 (modulo n 5))))

(define (flt N)
  (filter divisibility (seq (- N 1))))

(define (sum n)
  (fold + 0 (flt n)))

(define (euler-1 n)
  (sum n))

(define (rdc fn acc n)
  (if (<= n 0) acc
      (rdc fn (fn acc n) (1- n))))

(define (euler-1a n)
  (rdc (lambda (acc m)
       (if (divisibility m) (+ acc m)
	   acc))
       0 (1- n)))

(define (fib1 n)
  (cond
   ((= n 0) 1)
   ((= n 1) 1)
   (else (+ (fib1 (- n 1)) (fib1 (- n 2))))))

(define fib-seq (make-hash-table (expt 10 5)))
(define (add-fib-seq k v)
  (hashq-set! fib-seq k v)
  v)

(define (fib1a n)
  (let
      ((m (hashq-get-handle fib-seq n)))
    (cond
     (m (cdr m))
     ((= n 0) (add-fib-seq n 1))
     ((= n 1) (add-fib-seq n 1))
     (else (add-fib-seq n (+ (fib1a (- n 1)) (fib1a (- n 2))))))))

(define (gen-fib-seq p fn acc n)
  (let ((cur (fib1a n)))
    (if (p cur) acc
      (gen-fib-seq p fn (fn cur acc) (1+ n)))))

(define (euler-2 Xlim)
  (gen-fib-seq
   (lambda (x) (> x Xlim))
   (lambda (x acc)
     (if (even? x) (+ x acc)
	 acc))
   0
   1))

(define (devides? n m)
  (= 0 (remainder n m)))

(define (start-attempt n)
  (if (even? n) (quotient n 2)
      (- (quotient n 2) 1)))

(define (max-prime n m step)
  (if (is-prime-factor? n m) m
      (max-prime n (- m step) step)))

(define (euler-3 n)
  (let ((step (if (even? n) 1 2)))
    (max-prime n (start-attempt n) step)))

(define (find-devisor n d )
  (cond
   ((> (* d d) n) n)
   ((devides? n d) d)
   (else (find-devisor n (1+ d)))))

(define (smallest-devisor n)
  (find-devisor n 2))

(define (prime?  n)
  (= n (smallest-devisor n)))

(define (greatest-devisor n)
  (let ((d (smallest-devisor n)))
    (if (= d n) n
	(greatest-devisor (quotient n d)))))

(define big-number 600851475143)
