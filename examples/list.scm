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

(println (reverse (list 1 2 3 4 5)))

(println (reverse (fizzbuzz 100)))


(length (list 1 2 3 4 5))

(list? (list 1 2 3 4 5))
(list? 3)
(list? nil)



