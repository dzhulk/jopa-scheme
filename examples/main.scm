;; (define (fib n)
;;     (if (<= n 2)
;;         1
;;         (+ (fib (- n 1))
;;            (fib (- n 2)))));

;; (define (pow2 n) (* n n));

;; (println "Fib 14 is" (fib 5));

;; (println "16^2 is" (pow2 16));


;; (define (loop i n)
;;   (if (< i n) (do
;;                 (println i)
;;                 (loop (+ 1 i) n))))
;;
;; (loop 0 3)

(define (fizzbuzzaux n curr acc)
    (if (< curr n)
        (fizzbuzzaux n (+ curr 1)
            (if (= (% curr 15) 0)
                (cons "FizzBuzz" acc)
                (if (= (% curr 3) 0)
                    (cons "Fizz" acc)
                    (if (= (% curr 5) 0)
                        (cons "Buzz" acc)
                        (cons curr acc)))))
        acc))



(define (fizzbuzz n)
    (fizzbuzzaux (+ n 1) 1 nil))

(fizzbuzz 3)
