
(define (test a l)
  (if (list? l)
    (println "got list")
    (println "got something other")))

(test "should be ok" (list 1 2 4))

(test "should be ok" nil)

(test "should not work") # should not work


(define (test2 l)
  (if (list? l)
    (println "got list")
    (println "got something other")))

(test2 1)
(test2)

