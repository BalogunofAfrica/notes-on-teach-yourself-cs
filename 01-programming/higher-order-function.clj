(comment
  "First implementation of sum of an integer, with a constant inreament of 1"
  (defn sum-int [a b] (if (> a b) 0 (+ a (sum-int (inc a) b))))
  (sum-int 1 2)
  "First implementation of sum of an squares, with a constant inreament of 1"
  (defn sum-sq [a b] (if (> a b) 0 (+ (Math/pow a 2) (sum-sq (inc a) b))))
  (sum-sq 1 4))


(comment
  "Re-implementing the above by havng a general abstraction for all sums"
  (defn sum
    [term a b next]
    (if (> a b) 0 (+ (term a) (sum term (inc a) b next))))
  "Then we can have a new implementation for sum-int like so:"
  (defn sum-int [a b] (sum (fn [x] x) a b #(inc %)))
  (sum-int 1 3)
  "We can also have a new implementation for sum-sq like so:"
  (defn sum-sq [a b] (sum #(Math/pow % 2) a b #(inc %)))
  (sum-sq 1 4))
