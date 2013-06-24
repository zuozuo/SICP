; 1.2.1 Example Factorial
(define (factorial n) 
  (if (= n 1)
      1
      (* n (factorial (- n 1)))
  ))
(display (factorial 4))
(newline)



; Exercise 1.6. 
; Alyssa P. Hacker doesn't see whyifneeds to be provided as a special form. ``Why can't I just define it as an ordinary procedure in terms of 
; cond?'' she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
; Eva demonstrates the program for Alyssa:
(new-if (= 2 3) 0 5)
; 5
(new-if (= 1 1) 0 5)
0
; Delighted, Alyssa uses new-if to rewrite the square-root program:
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
; What happens when Alyssa attempts to use this to compute square roots? Explain.

; Answer 1.6
(define (square x) (* x x))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(display (sqrt 9))
;
; When you run the code above, you will come across a infinite loop. According to the substitution model for procedure application: 
; To apply a compound procedure to agruments, evaluate the body of the procedure with each formal parameter replaced by the correspinding 
; argument. So when the new-if procedure was called in sqrt-iter with two arguments guess and (sqrt-iter (improve guess x)), the 
; interpreter will firstly evaluate them , when it try to evaluate (sqrt-iter (improve guess x)), problem happens, because sqrt-iter 
; will never return unless new-if return. 


; Exercise 1.7
; The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers. 
; Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate 
; for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers. An alternative 
; strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a 
; very small fraction of the guess. Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers?
(define (good-enough? guess pre_guess) 
  (< (abs (- pre_guess guess)) 0.001))
(define (sqrt-iter guess pre_guess x) 
  (if (good-enough? guess pre_guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

