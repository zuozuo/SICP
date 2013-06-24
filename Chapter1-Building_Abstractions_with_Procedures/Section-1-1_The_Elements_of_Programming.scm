; Exercise 1.2
; Translate the following expression into prefix form:
; { 5 + 4 + [ 2 - (3 - (6 + 4/3)) ] }/[3 * (6-2) * (2-7)]
;
(display (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3))))) (* 3 (- 6 2) (- 2 7))))
(newline)



; Exercise 1.3
; Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
; 
(define (square_sum_2_larger_of_3 x y z) 
  (cond ((and (> x y) (> z y)) (+ (* x x) (* z z)))
        ((and (> x z) (> y z)) (+ (* x x) (* y y)))
        (else (+ (* z z) (* y y))))
        ; ((and (> y x) (> z x)) (+ (* z z) (* y y))))
  )
(display (square_sum_2_larger_of_3 1 2 3))



; Exercise 1.5 
; Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is usingapplicative-order evaluation or normal-order 
; evaluation. He defines the following two procedures:
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
; Then he evaluates the expression
;
; (test 0 (p))
;
; What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter 
; that uses normal-order evaluation? Explain your answer. (Assume that the evaluation rule for the special form if is the same whether the 
; interpreter is using normal or applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate 
; the consequent or the alternative expression.)

; Answer 1.5
; For an interpreter using applicative-order evaluation the result will be 0, however, for an interpreter doing normal-order evaluation it will 
; fall in an infinite loop.



; 1.1.7 Square Root by Newton's Method

(define (sqrt_root num guess precision) 
  (if (<= (abs (- num (* guess guess))) precision)
      guess
      (sqrt_root num (/ (+ (/ num guess) guess) 2) precision)
  ))
(newline)
(display (sqrt_root 2 1.0 0.00000000000001))


