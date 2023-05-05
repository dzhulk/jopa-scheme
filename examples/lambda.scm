(define (adder n)
  (lambda (i) (+ n i)))

((lambda (i) (+ 1 i)) 2)

(define add3 (adder (+ 1 4)))

(println i)

(println ((lambda (i) (+ 2 i)) 3))
(println add3)

(add3 2)

(define (inner gn)
  (println (gn 25)))

(define (perform fn)
  (do
    (println (fn 9))
    (inner fn)
  ))

(perform (lambda (n) (* n n)))


(define (map-in fn lst acc)
  (if (nil? lst) acc
    (map-in fn (cdr lst) (conj (fn (car lst)) acc))))

(define (map fn lst) (map-in fn lst nil))

(map (lambda (x) (* x x)) (list 2 3 4 5 6))




