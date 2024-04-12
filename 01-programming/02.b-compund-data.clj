(comment
  (do (def pair (cons 1 [2])) (last pair)))

;; Talking about abstractions and substitution by using rational numbers as an
;; example
(comment
  (do (defn gcd [a b] (if (zero? b) (Math/abs a) (recur b (mod a b))))
      (defn make-rat [n d] (let [g (gcd n d)] (cons (/ n g) [(/ d g)])))
      (defn numer [n] (first n))
      (defn denom [n] (last n))
      ;; Computing
      (def a (make-rat 1 2))
      (def b (make-rat 1 4))
      (def ans (+ a b))
      (println ans)))