
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
(require rackunit)
;; put your code below


(define (sequence low high stride)
  (cond [(> low high) null]
        [#t (cons low (sequence (+ low stride) high stride))]))

;; (string-append-map '("haha" "hehe") "suffix")
(define (string-append-map xs suffix)
  (map (lambda (str)
        (string-append str suffix))
  xs))

;; (list-nth-mod '(3 5 7 9 11) 2)
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
    null
    (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x) 
            (if (= (remainder x 5) 0)
              (cons (- x) (lambda () (f (+ x 1))))
              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) 
            (if (= (remainder x 2) 1)
              (cons "dan.jpg" (lambda () (f (+ x 1))))
              (cons "dog.jpg" (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) stream-add-zero (cdr (s)))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
                (cons (cons (list-nth-mod xs x) (list-nth-mod ys x) (lambda () (f (+ x 1))))))])
  (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (x)
                (cond [(= x len) #f]
                      [(let ([i (vector-ref vec x)])
                         (and (cons? i) (equal? v (car i)))) (vector-ref vec x)]
                      [#t (f (+ x 1))]))])
  (f 0)))

(define (cached-assoc xs n)
  (let ([memo (make-vector n #f)]
        [index 0])
    (lambda (v) 
      (let ([r1 (vector-assoc v memo)])
        (if (equal? r1 #f)
          (let ([r2 (assoc v xs)])
            (begin
              (vector-set! memo index r2)
              (set! index (remainder (+ index 1) n)))
              r2)
          r1)))))
  