(use-modules (srfi srfi-1))

(define (seq n)
  (if (= n 0) '()
      (cons n (mk-l (- n 1)))))

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
  (if (= 0 n) acc
      (rdc fn (fn acc n) (- n 1))))

(define (euler-1a n))
