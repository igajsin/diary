(define-module (euler euler11)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-moduler (srfi srfi-1))

(define file.dat "/home/ugoday/src/diary/euler/euler11.data")

(define (read-lines acc)
  (let ((line (read-line)))
    (if (eof-object? line) acc
	(read-lines
	 (cons (map string->number
		    (string-split line #\Space))
	       acc)))))

(define l11 (with-input-from-file file.dat (lambda () (read-lines '()))))

(define (best-prod lst)
  (car (fold (lambda (x acc)
	(let* ((best (car acc))
	       (challengers (cons x (cdr acc)))
	       (attempt (reduce * 0 challengers)))
	  (cons
	   (if (> attempt best) attempt
	       best)
	   (list-head challengers (min 3 (length challengers))))))
      '(1)
      lst)))

(define (transform lst)
  (fold (lambda (x acc)
	  (map (lambda (y z) (cons y z)) x acc))
	(make-list (length lst))
	lst))


(define (half-diag proc lst)
  (fold (lambda (x acc)
	(let* ((k (length (car acc)))
	       (y (append (list-tail x k) (make-list k))))
	  (map (lambda (z1 z2)
		 (if (null? z1) z2
		     (cons z1 z2))) y acc)))
      (make-list (length lst))
      (proc lst)))

(define (diags lst)
  (append
   (half-diag (lambda (x) x) lst)
   (half-diag reverse lst)
   (half-diag (lambda (x) (map reverse x)) lst)
   (half-diag (lambda (x) (reverse (map reverse x))) lst)))

(fold max 1
      (append-map
       (lambda (x) (map best-prod x))
       (list l11 (transform l11) (diags l11))))
