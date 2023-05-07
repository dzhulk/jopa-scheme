
(define big-list-one (map (lambda (i) (* i 2)) (range 0 1000)))

(define big-list (map (lambda (i) (* i 2)) (range 0 1000)))

(append big-list big-list-one)
