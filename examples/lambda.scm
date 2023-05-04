(define (adder n)
  (lambda (i) (+ n i)))

;; ((lambda (i) (+ 1 i)) 2)

(define add3 (adder (+ 1 4)))

;; (println i)

;; (println ((lambda (i) (+ 2 i)) 3))
;; (println add3)
;;
(add3 2)
