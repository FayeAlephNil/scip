; EXERCISE 1.7 and 1.8
(define tolerance 0.001)

(define (good-enough? f guess x)
  (< (/ (abs (- (f guess x) guess)) guess) tolerance))

(define (improve-iter improver guess x)
  (if (good-enough? improver guess x)
      guess
      (improve-iter improver (improver guess x) x)))

(define (square x) (* x x))

(define (pown n x)
  (if (= n 0)
      1
      (* x (pown (- n 1) x))))

(define (average x y) (/ (+ x y) 2))

(define (improve-sqrt guess x) (average (/ x guess) guess))

(define (improve-cbrt guess x) (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (sqrt-iter guess x) (improve-iter improve-sqrt guess x))
(define (cbrt-iter guess x) (improve-iter improve-cbrt guess x))

; (sqrt-iter 1 4) ==> 3281/1640

; EXERCISE 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; (A 1 10) ==> 1024
; (A 2 4) ==> 65536
; (A 3 3) ==> 65536

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

; (k 5) ==> 125
; (g (g 4)) ==> 65536
; (h 4) ==> 65536
; (g (g 5)) ==> 4294967296
; (f 1) ==> 2
; (f 5) ==> 10
; (g 1) ==> 2
; (g 2) ==> 4
; (g 3) ==> 8
; (f n) = 2 * n
; (g n) = 2^n
; (h n) = 2^...^2 (n times)

; EXERCISE 1.11

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

; (f-rec -5) ==> -5
; (f-rec 5) ==> 25
; (f-rec 6) ==> 59

(define (f-iter-go a b c count)
  (if (= count 2)
      c
      (f-iter-go b c (+ c (* 2 b) (* 3 a)) (- count 1))))

(define (f-iter n)
  (if (< n 3)
      n
      (f-iter-go 0 1 2 n)))

; (f-iter -5) ==> -5
; (f-iter 5) ==> 25
; (f-iter 6) ==> 59

; EXERCISE 1.12

(define (pascal n k)
  (cond ((= n k) 1)
        ((< n k) 0)
        ((< k 0) 0)
        ((= k 0) 1)
        (else (+ (pascal (- n 1) (- k 1)) (pascal (- n 1) k)))))

; (pascal 0 0) ==> 1
; (pascal 2 1) ==> 2
; (pascal 4 3) ==> 4
; (pascal 4 2) ==> 6
