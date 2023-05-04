((lambda (a) (+ 1 a)) 1)
((lambda (a b) (+ b a)) 2 (+ 1 2))

(lambda (a) (+ 1 a))

(define (incrementor n y)
  (lambda (i) (+ n i y)))

((incrementor 2 (+ 1 1)) 2)
