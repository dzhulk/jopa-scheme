(define (append list1 list2)
        (if (nil? list1) list2
            (cons (car list1) (append (cdr list1) list2))))

(define (reverse l)
  (if (nil? l)
    nil
    (append (reverse (cdr l)) (list (car l)))))

(define (map fn lst)
  (do
    (define (mapaux-tc fn lst acc)
      (if (nil? lst) acc
        (mapaux-tc fn (cdr lst) (conj (fn (car lst)) acc))))
      (mapaux-tc fn lst nil)))

(define (reduce fn coll init)
  (do
    (define (reduce-tc)
      (if (nil? coll)
        init
        (reduce-tc fn (cdr coll) (fn (car coll) init))))
    (reduce-tc fn coll init)))

(define (filter pred coll)
  (do
    (define (filteraux-tc pred coll acc)
      (if (nil? coll)
        acc
        (if (pred (car coll))
          (filteraux-tc pred (cdr coll) (conj (car coll) acc))
          (filteraux-tc pred (cdr coll) acc))))
    (filteraux-tc pred coll nil)))

(define (avg lst)
  (/
    (reduce (lambda (el acc) (+ el acc)) lst 0.0)
    (length lst)))

(define (min lst)
   (reduce (lambda (el m) (if (< el m) el m)) (cdr lst) (car lst)))

(define (max lst)
   (reduce (lambda (el m) (if (> el m) el m)) (cdr lst) (car lst)))


(define (range from to)
  (do
    (define (rangeaux-tc i acc)
      (if (= i to)
        acc
        (rangeaux-tc (+ i 1) (conj i acc))))
     (rangeaux-tc from nil)))
