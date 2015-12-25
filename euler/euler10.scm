(define-module (euler euler10)
  #:use-module (euler util))

(define (fill v)
  (do ((i (1- (vector-length v)) (1- i)))
      ((= i 1))
    (vector-set! v i i))
  v)

(define bv (fill (make-vector (* 2 (expt 10 6)) 0)))
(define res)

(define (remove-not-primes v)
  (do ((i 2 (1+ i)))
      ((> i (1- (vector-length v))))
    (when (not (= i 0))
	(strike v i))))

(define (strike v n)
  (do ((i (* n 2) (+ i n)))
      ((> i (1- (vector-length v))))
    (vector-set! v i 0)))

(define (vector-sum v)
  (let ((sum 0))
    (do ((i 0 (1+ i)))
	((> i (1- (vector-length v))))
      (set! sum (+ sum (vector-ref v i))))
    sum))
