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


(define (fib n)
    (if (<= n 2)
        1
        (+ (fib (- n 1))
           (fib (- n 2)))))

(fib 20)

(define nums (list 1 2 3 4 5 6 7 8 9 10))

;; select even elements
(define even
  (filter
    (lambda (el) (= (% el 2) 0))
    nums))

(println even)

;; calculate list product
(reduce (lambda (el acc) (* el acc)) nums 1)

;; hof test
(define (even? el) (= (% el 2) 0))

(filter even? nums)

(define odd? (lambda (el) (not (even? el))))

(filter odd? nums)

(define (avg lst)
  (/
    (reduce (lambda (el acc) (+ el acc)) lst 0)
    (length lst)))

(avg nums)
