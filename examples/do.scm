
;; // TODO: detect cyclic references
;; //
(define (say name) (println name))

;; (say name) cyclic example

(say "hello")
