(define (fib n)
    (if (<= n 2)
        1
        (+ (fib (- n 1))
           (fib (- n 2)))));

(define (pow2 n) (* n n));


(println "Fib 14 is" (fib 14));

(println "16^2 is" (pow2 16));

