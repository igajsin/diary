#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path "/home/ugoday/src/diary")

(use-modules (euler euler10))

(define (main args)
  (write (vector-sum (string->number (cadr args)))))
