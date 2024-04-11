(comment
  "First implementation of sum of an integer, with a constant inreament of 1"
  (do (defn sum-int [a b] (if (> a b) 0 (+ a (sum-int (inc a) b))))
      (sum-int 1 2)
      ;; "First implementation of sum of an squares, with a constant
      ;; inreament of 1"
      (defn sum-sq [a b] (if (> a b) 0 (+ (Math/pow a 2) (sum-sq (inc a) b))))
      (sum-sq 1 4)))


(comment
  "Re-implementing the above by havng a general abstraction for all sums"
  (do (defn sum
        [term a b next]
        (if (> a b) 0 (+ (term a) (sum term (inc a) b next))))
      ;; "Then we can have a new implementation for sum-int like so:"
      (defn sum-int [a b] (sum (fn [x] x) a b #(inc %)))
      (sum-int 1 3)
      ;; "We can also have a new implementation for sum-sq like so:"
      (defn sum-sq [a b] (sum #(Math/pow % 2) a b #(inc %)))
      (sum-sq 1 4)))


(comment
  "computing derivative"
  (do (defn derivative [f x h] (/ (- (f (+ x h)) (f x)) h))
      (defn square [x] (* x x))
      (defn square-prime [x] (derivative square x 0.000111))
      (println (square-prime 8))))

(comment
  "Implementing Netwons Method"
  (defn newtons-method
    "Calculates a root of a function using Newton's method."
    [f f-prime x0 tolerance max-iterations]
    (loop [x x0
           iter 0]
      (let [fx (f x)
            fpx (f-prime x)
            next-x (- x (/ fx fpx))]
        (if (or (< (Math/abs (- next-x x)) tolerance) (>= iter max-iterations))
          (do (println "newtons-method took" iter "iterations") next-x)
          (recur next-x (inc iter)))))))


(comment
  "Using newton method to possibly derive a value for the square root of x"
  "Let's say x is 10"
  (do (defn f [x] (- (* x x) 1e50))
      (defn f-prime [x] (derivative f x 1e-6))
      (println (newtons-method f f-prime 1 1e-6 100))))

(comment
  (defn average
    [numbers]
    (when (seq numbers) (/ (reduce + numbers) (count numbers))))
  (defn average-damp [f] (fn [x] (average [(f x) x]))))
