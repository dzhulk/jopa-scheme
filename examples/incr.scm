
;; (= (+ 2 3 (- 10 5)) 10)
(= (- 8 1 1 1) 5)
(= (* 12 12 12) 1728)
(= (> 9 7 6 4 3))

(if (= "tert" "test") (println "it is true") (println "it is a lie"))

(do (println "foo") (println "bar" "baz") (join 1 2 3))


(define (loop f t)
    (if (< f t)
        (loop (+ f 1) t)))

(loop 1 10)
