
(define (fizzbuzzaux n curr acc)
    (if (< curr n)
        (fizzbuzzaux n (+ curr 1)
            (if (= (% curr 15) 0)
                (conj "FizzBuzz" acc)
                (if (= (% curr 3) 0)
                    (conj "Fizz" acc)
                    (if (= (% curr 5) 0)
                        (conj "Buzz" acc)
                        (conj curr acc)))))
        acc))

(define (fizzbuzz n)
    (fizzbuzzaux (+ n 1) 1 nil))


(println (fizzbuzz 100))

(define (fib n)
    (if (<= n 2)
        1
        (+ (fib (- n 1))
           (fib (- n 2)))))

(fib 20)
