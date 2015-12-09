(define-module (euler util)
  #:export (seq oseq get-order elten is-devisor?))

(define (is-devisor? n k)
  (= 0 (remainder n k)))

(define* (seq n #:optional m)
  (if (= n m) '()
      (cons n (seq (- n 1) m))))

(define (oseq start end)
  (letrec
      ((oseq1 (lambda (stop? next start end)
			(if (stop? start end) '()
			    (cons start (oseq1 stop? next (next start) end))))))
    (if (<= start end) (oseq1 > 1+ start end)
	(oseq1 < 1- start end))))

(define (get-order1 n m)
  (if (= 0 (quotient n (expt 10 m))) (1- m)
      (get-order1 n (1+ m))))

(define (get-order n)
  (get-order1 n 0))


(define (elten n m)
  (if (> m (get-order n)) (raise "too big m")
      (quotient (remainder n (expt 10 (1+ m))) (expt 10 m))))

