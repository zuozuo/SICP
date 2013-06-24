(define (good-enough? guess pre_guess) 
  (< (abs (- pre_guess guess)) 0.001))
(define (sqrt-iter guess pre_guess x) 
  (if (good-enough? guess pre_guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (square x) (* x x))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x x))

(display (square 0.03))
