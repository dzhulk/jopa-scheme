(do
    (println "foo")
    (println "bar" "baz")
    (concat 1 2 3))


(define (loop f t)
    (if (< f t)
        (do
            (println f)
            (loop (+ f 1) t))))

(loop 1 10)
