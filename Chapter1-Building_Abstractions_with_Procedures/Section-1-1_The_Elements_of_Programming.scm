; Exercise 1.2
; Translate the following expression into prefix form:
; { 5 + 4 + [ 2 - (3 - (6 + 4/3)) ] }/[3 * (6-2) * (2-7)]
;
(display (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3))))) (* 3 (- 6 2) (- 2 7))))
(newline)



; Exercise 1.3
; Define a procedure that takes three numbers as arguments and returns the sum 
; of the squares of the two larger numbers.
; 
(define (square_sum_2_larger_of_3 x y z) 
  (cond ((and (> x y) (> z y)) (+ (* x x) (* z z)))
        ((and (> x z) (> y z)) (+ (* x x) (* y y)))
        (else (+ (* z z) (* y y))))
        ; ((and (> y x) (> z x)) (+ (* z z) (* y y))))
  )
(display (square_sum_2_larger_of_3 1 2 3))



; Exercise 1.5 
; Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is 
; usingapplicative-order evaluation or normal-order evaluation. He defines the following two procedures:
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
; Then he evaluates the expression
;
; (test 0 (p))
;
; What behavior will Ben observe with an interpreter that uses applicative-order evaluation? 
; What behavior will he observe with an interpreter that uses normal-order evaluation? 
; Explain your answer. (Assume that the evaluation rule for the special form if is the same whether the 
; interpreter is using normal or applicative order: The predicate expression is evaluated first, 
; and the result determines whether to evaluate the consequent or the alternative expression.)

; Answer 1.5
; For an interpreter using applicative-order evaluation the result will be 0, however, 
; for an interpreter doing normal-order evaluation it will fall in an infinite loop.



; 1.1.7 Square Root by Newton's Method

(define (sqrt_root num guess precision) 
  (if (<= (abs (- num (* guess guess))) precision)
      guess
      (sqrt_root num (/ (+ (/ num guess) guess) 2) precision)
  ))
(newline)
(display (sqrt_root 2 1.0 0.00000000000001))



; Exercise 1.6. 
; Alyssa P. Hacker doesn't see whyifneeds to be provided as a special form. 
; ``Why can't I just define it as an ordinary procedure in terms of cond?`` she asks. 
; Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:
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
; When you run the code above, you will come across a infinite loop. 
; According to the substitution model for procedure application: 
; To apply a compound procedure to agruments, evaluate the body of the procedure 
; with each formal parameter replaced by the correspinding argument. 
; So when the new-if procedure was called in sqrt-iter with two arguments guess and (sqrt-iter (improve guess x)), 
; the interpreter will firstly evaluate them , when it try to evaluate (sqrt-iter (improve guess x)), 
; problem happens, because sqrt-iter will never return unless new-if return. 


; Exercise 1.7
; The good-enough? test used in computing square roots will not be very effective for finding the square roots 
; of very small numbers. Also, in real computers, arithmetic operations are almost always performed with 
; limited precision. This makes our test inadequate for very large numbers. Explain these statements, 
; with examples showing how the test fails for small and large numbers. An alternative strategy for implementing 
; good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a 
; very small fraction of the guess. Design a square-root procedure that uses this kind of end test. 
; Does this work better for small and large numbers?
;
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



; Exercise 1.8
; Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x, 
; then a better approximation is given by the value (x/y+2*y)/3 
; Use this formula to implement a cube-root procedure analogous to the square-root procedure.
;
(define (cube x) 
  (cube-iter 1.0 x x))
(define (cube-iter guess pre_guess x) 
  (if (good-enough? guess pre_guess)
      guess 
      (cube-iter (improve guess x) guess x)))
(define (square x) (* x x))
(define (improve guess x) 
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (good-enough? guess pre_guess) 
  (< (abs (- pre_guess guess)) 0.001))
(display (cube 27))


