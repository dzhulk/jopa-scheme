(define (append list1 list2)
        (if (nil? list1) list2
            (cons (car list1) (append (cdr list1) list2))))

(define (reverse l)
  (if (nil? l)
    nil
    (append (reverse (cdr l)) (list (car l)))))

(define (map fn lst)
  (do
    (define (mapaux fn lst acc)
      (if (nil? lst) acc
        (mapaux fn (cdr lst) (conj (fn (car lst)) acc))))
      (mapaux fn lst nil)))

(define (reduce fn coll init)
  (if (nil? coll)
    init
    (reduce fn (cdr coll) (fn (car coll) init))))

(define (filter pred coll)
  (do
    (define (filteraux pred coll acc)
      (if (nil? coll)
        acc
        (if (pred (car coll))
          (filteraux pred (cdr coll) (conj (car coll) acc))
          (filteraux pred (cdr coll) acc))))
    (filteraux pred coll nil)))
