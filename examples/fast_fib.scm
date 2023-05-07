(define (fib n)
  (do
    (define (fibaux n acc)
      (if (= (length acc) n)
        acc
        (fibaux
          n
          (cons (+
                  (car acc)
                  (car (cdr acc)))
                acc))))
      (reverse (fibaux n (list 1 0)))))

(fib 50)
