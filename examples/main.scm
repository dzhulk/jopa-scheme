(define (fib n)
    (if (<= n 2)
        1
        (+ (fib (- n 1))
           (fib (- n 2)))));

(define (pow2 n) (* n n));


(println "Fib 14 is" (fib 14));

(println "16^2 is" (pow2 16));


(define (fizzbuzzaux n curr)
    (if (< curr n)
        (do
            (if (= (% curr 15) 0)
                (println "FizzBuzz")
                (if (= (% curr 3) 0)
                    (println "Fizz")
                    (if (= (% curr 5) 0)
                        (println "Buzz")
                        (println curr))))
            (fizzbuzzaux n (+ curr 1)))));

(define (fizzbuzz n)
    (fizzbuzzaux (+ n 1) 1));

(fizzbuzz 100);


