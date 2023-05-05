
(define (append list1 list2)
        (if (nil? list1) list2
            (cons (car list1) (append (cdr list1) list2))))

(define (reverse l)
  (if (nil? l)
    nil
    (append (reverse (cdr l)) (list (car l)))
  )
)

(define (map-aux fn lst acc)
  (if (nil? lst) acc
    (map-aux fn (cdr lst) (conj (fn (car lst)) acc))))

(define (map fn lst) (map-in fn lst nil))
