(define (adder n)
  (lambda (i) (+ n i)))

(define add3 (adder (+ 1 1)))



;; (println ((lambda (i) (+ 2 i)) 3))
;; (println add3)
;;
(add3 2)
