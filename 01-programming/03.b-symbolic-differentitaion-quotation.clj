

(comment
  (do (def one-four (list 1 2 3 4))
      (println one-four)
      (first (rest (rest one-four))))
  (map inc '(1 2 3)))

(defn atom?
  "Returns true if x is an 'atomic' value, not a collection."
  [x]
  (not (or (coll? x) (sequential? x))))


(defn constant? [exp var] (and (atom? exp) (not= exp var)))
(defn same-var? [exp var] (and (atom? exp) (= exp var)))
(defn sum? [exp var] (and (not (atom? exp)) (= (first exp) '+)))
(defn make-sum [a1 a2] (list '+ a1 a2))


(defn derivative
  [exp var]
  (cond (constant? exp var) 0
        (same-var? exp var) 1
        :else "none"))


