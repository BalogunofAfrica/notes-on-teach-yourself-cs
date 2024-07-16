;; Sieve of Eratosthenes
(defn sieve
  [s]
  (cons (first s)
        (lazy-seq (sieve (filter #(not= 0 (mod % (first s))) (rest s))))))

(take 10 (sieve (iterate inc 2)))
