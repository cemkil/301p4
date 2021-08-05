#lang racket

;;;;;;;;;;;;;;;;;;;;;;; TASK 4 ;;;;;;;;;;;;;;;;;;;;;;;;

(define fibonacci/k (lambda (n cont)
                  (cond ((eq? n 0) (cont 0))
                        ((eq? n 1) (cont 1))
                        (else (cont(+ (fibonacci/k (- n 1) (lambda (x) x))
                                      (fibonacci/k (- n 2) (lambda (x) x))) )))) 
                    )
(define fibonacci (lambda (n)
                    (fibonacci/k n (lambda (x) x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; i:    1  2  3  4  5  6  7  8  9  10 11 ...
; f(i): 1  1  2  3  5  8  13 21 34 55 ...

;; Tests
(display (fibonacci 4)) ; should output 3
(display  "\n")
(display (fibonacci 7)) ; should output 13
(display  "\n")
(display (fibonacci 8)) ; should output 21
(display  "\n")