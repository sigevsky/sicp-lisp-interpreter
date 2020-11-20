(define dx 1)
(define (deriv g) (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(print "hello")
