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

(define (append list1 list2)
        (if (nil? list1) list2
            (cons (car list1) (append (cdr list1) list2))))

(define (reverse l)
  (if (nil? l)
    nil
    (append (reverse (cdr l)) (list (car l)))
  )
)

(println (reverse (list 1 2 3 4 5)))

(println (reverse (fizzbuzz 100)))


(length (list 1 2 3 4 5))

(list? (list 1 2 3 4 5))
(list? 3)
(list? nil)



