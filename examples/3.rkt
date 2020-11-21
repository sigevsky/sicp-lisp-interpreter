(define (hello lang)
  (cond ((eq? lang "fr") "bonjour")
        ((eq? lang "en") "hello")
        ((eq? lang "ru") "privet")
        (else "|||||||||")))

(display (hello "ru"))

(display (let* ((x 1) (y (+ x 1))) (+ x y)))


(display (and #t (= 1 1) (eq? "ac" "ac")))
(display (and #t (eq? "ac" "ac") (= 4 5)))
(display (or #f (eq? "a1" "ac") (= 4 5) #t))
