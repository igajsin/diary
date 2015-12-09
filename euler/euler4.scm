(define-module (euler euler4)
  #:use-module (euler util)
  #:use-module (srfi srfi-1)
  #:export (p4p))

(define (ps4p pn)
  "palindrome numbers from palindrome number"
  (map (lambda (n) (p4p pn n)) (oseq 9 1)))

(define (p4p pn n)
  (+ n (* 10 pn) (* n (expt 10 (+ (get-order pn) 2)))))

(define palindroms (append-map ps4p  (append-map ps4p (append-map ps4p (oseq 0 9)))))
(define pp (filter (lambda (x) (< x (* 999 999))) palindroms))

(define (find-devisor1 n k)
  (if (is-devisor? n k) k
      (find-devisor1 n (1- k))))

(define (find-devisor n)
  (find-devisor1 n (min 999 (quotient n 2))))

(define (is-3d-number? n)
  (and (> n 100) (< n 999)))

(define (firsten n)
  (elten n (get-order n)))

(define (lasten n)
  (elten n 0))

(define (stripen n)
  (let* ((fo (expt 10 (get-order n))))
    (quotient (remainder n fo) 10)))

(define (is-palindrome? n)
  (cond
   ((> lborder uborder) '())
   ((is-valide-palindrome? uborder) (cons uborder (palist lborder (1- uborder))))
   (#t (palist lborder (1- uborder)))))

(define (is-palindrome? num)
  (let ((ord (get-order num)))
    (fold (lambda (v acc) (and v acc))
	  #t
	  (map
	   (lambda (x) (= (elten num x)
			  (elten num (- ord x))))
	   (oseq 0 (quotient ord 2))))))
