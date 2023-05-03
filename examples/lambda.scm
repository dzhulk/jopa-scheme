
# (lambda (a) (+ 1 a))

(define (incrementor n)
  (lambda (i) (+ n i)))

# ((incrementor 2) 2)
