(define (hello lang)
  (cond ((eq? lang "fr") "bonjour")
        ((eq? lang "en") "hello")
        ((eq? lang "ru") "privet")
        (else "|||||||||")))

(display (hello "ru"))

(display (let* ((x 1) (y (+ x 1))) (+ x y)))
