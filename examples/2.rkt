(define dx 1)
(define (deriv g) (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define x (readLn))
(print x)
