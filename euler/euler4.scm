(define-module (euler euler4)
  #:use-module (euler util)
  #:use-module (srfi srfi-1))

(define (is-devisor? n k)
  (= 0 (remainder n k)))

(define (find-devisors-in-range x lborder uborder)
  (cond
   ((> lborder uborder) '())
   ((is-devisor? x uborder) (cons uborder (find-devisors-in-range x lborder (1- uborder))))
   (#t (find-devisors-in-range x lborder (1- uborder)))))

(define (is-n3? n)
  (let* ((devisors (find-devisors-in-range n 100 999))
	 (subdevisors (map (lambda (x) (/ n x)) devisors)))
    (not (null?
	  (filter (lambda (x) (memq x devisors)) subdevisors)))))

(define (is-valide-palindrome? n)
  (and (is-palindrome? n)
       (> n 10000) (< n 998001)
       (is-n3? n)))

(define (palist lborder uborder)
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
