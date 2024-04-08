(defn sum-int [a b] (if (> a b) 0 (+ a (sum-int (inc a) b))))

(sum-int 1 2)

(comment
  (print "Hello"))


(defn sum-sq [a b] (if (> a b) 0 (+ (Math/pow a 2) (sum-sq (inc a) b))))

(sum-sq 1 4)

(defn sum [term a b next] (if (> a b) 0 (+ (term a) (sum term (inc a) b next))))

()