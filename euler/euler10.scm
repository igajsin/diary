(define-module (euler euler10)
  #:export (vector-sum))

(define (fill v)
  (do ((i (1- (vector-length v)) (1- i)))
      ((= i 1))
    (vector-set! v i i))
  v)

(define (remove-not-primes v)
  (do ((i 2 (1+ i)))
      ((> i (1- (vector-length v))))
    (when (not (= i 0))
	(strike v i))))

(define (strike v n)
  (do ((i (* n 2) (+ i n)))
      ((> i (1- (vector-length v))))
    (vector-set! v i 0)))

(define (vector-sum M)
  (let ((sum 0)
	(v (fill (make-vector M 0))))
    (remove-not-primes v)
    (do ((i 0 (1+ i)))
	((> i (1- (vector-length v))))
      (set! sum (+ sum (vector-ref v i))))
    sum))

