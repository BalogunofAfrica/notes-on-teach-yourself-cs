(comment
  "Doing factorials the mathematical way, without assignments (functional version)"
  (do (defn fact [n] (loop [m 1 i 1] (if (> i n) m (recur (* i m) (+ i 1)))))
      (fact 7)))

(comment
  "Doing factorials with assignments (imperative version)"
  (do (defn fact
        [n]
        (let [m (atom 1)
              i (atom 1)]
          ((fn loop! []
             (if (> @i n)
               @m
               (do (reset! m (* @i @m)) (reset! i (+ @i 1)) (loop!)))))))
      (fact 7)))

(comment
  "Trying to estimate PI using cesaro's method"
  (do (defn monte-carlo
        [trials experiment]
        (loop [remaining trials
               passed 0]
          (cond (= remaining 0) (/ passed trials)
                (experiment) (recur (dec remaining) (inc passed))
                :else (recur (dec remaining) passed))))
      (defn gcd [a b] (if (zero? b) (Math/abs a) (recur b (mod a b))))
      (defn rand [] (Math/round (* (Math/random) 10000)))
      (defn cesaro [] (= (gcd (rand) (rand)) 1))
      (defn estimate-pi [n] (Math/sqrt (/ 6 (monte-carlo n cesaro))))
      (estimate-pi 1000000)))