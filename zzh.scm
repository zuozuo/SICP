


























(define (multiply a b) 
  (multiply-iter a b 0))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (multiply-iter a b prod) 
  (display b)
  (newline)
  (display prod)
  (newline)
  (display "*************")
  (newline)
  (cond ((= b 0) prod)
        ((even? b) (multiply-iter (double a) (halve b) prod))
        (else (multiply-iter a (- b 1) (+ prod a)))))

(display (multiply 28 300))


















