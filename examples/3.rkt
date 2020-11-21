(define (hello lang)
  (cond ((eq? lang "fr") "bonjour")
        ((eq? lang "en") "hello")
        ((eq? lang "ru") "privet")
        (else "|||||||||")))

(print (hello "ruw"))
