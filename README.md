## Simple scheme like lang

How to run:
```bash
cargo run --bin jopc -- examples/lambda.scm
```

You can also run a REPL:
```bash
cargo run --bin repl
```

Built-ins:
```
+ - / *
> < >= <= =
nil?
list?
length
or and not
if - (if (cond) () ())
do - execute blocks of code, returns last value
cons - (cons el list) - push to front
conj - (cons el list) - append to list
car  - (car list) - get list head
cdr  - (cdr list) - get list tail
concat - (concat "string" 1 (list 1 2 3)) - join elements to string
println
define - define function or variable
list   - construct new list (list 1 2 3)
```

## Some examples
map:
```scheme
(define (map-in fn lst acc)
  (if (nil? lst)
    acc
    (map-in fn
            (cdr lst)
            (conj (fn (car lst)) acc))))

(define (map fn lst) (map-in fn lst nil))

(map (lambda (x) (* x x)) (list 2 3 4 5 6))
```

FizzBuzz:
```scheme
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
```

Fibonacci:
```scheme
(define (fib n)
    (if (<= n 2)
        1
        (+ (fib (- n 1))
           (fib (- n 2)))))

(fib 20)
```

Filter & reduce:
```scheme
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
```
