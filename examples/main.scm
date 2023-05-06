
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


(define (map-in fn lst acc)
  (if (nil? lst)
    acc
    (map-in fn
            (cdr lst)
            (conj (fn (car lst)) acc))))


(define (map fn lst) (map-in fn lst nil))

(define (filteraux pred coll acc)
  (if (nil? coll)
    acc
    (if (pred (car coll))
      (filteraux pred (cdr coll) (conj (car coll) acc))
      (filteraux pred (cdr coll) acc))))

(define (filter pred coll) (filteraux pred coll nil))

;; select even elements
(define even
  (filter
    (lambda (el) (= (% el 2) 0))
    (list 1 2 3 4 5 6 7 8 9 10)))

(println even)

(define (reduce fn coll init)
  (if (nil? coll)
    init
    (reduce fn (cdr coll) (fn (car coll) init))))

;; calculate list product
(reduce (lambda (el acc) (* el acc)) (list 1 2 4 6  8 10) 1)


