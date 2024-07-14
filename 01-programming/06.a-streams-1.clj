(defn noop [arg] arg)
(defn square [n] (* n n))
(def leaf-node? noop)
(def left-branch noop)
(def right-branch noop)
(def fib noop)

(defn sum-odds-square
  [tree]
  (if (leaf-node? tree)
    (if (odd? tree) (square tree) 0)
    (+ (sum-odds-square (left-branch tree))
       (sum-odds-square (right-branch tree)))))

(defn odd-fibs
  [n]
  (letfn [(next-fn [k]
            (if (> k n)
              '()
              (let [f (fib k)]
                (if (odd? f) (cons f (next-fn (inc k))) (next-fn (inc k))))))]
    (next-fn 1)))

(range 10 100 20)

(defn prime?
  [n]
  (cond (<= n 1) false
        (= n 2) true
        (even? n) false
        :else (not-any? #(zero? (rem n %)) (range 3 (inc (Math/sqrt n)) 2))))
(prime? 1009)
(first (rest (filter prime? (range 10000 1000000))))

(doseq [x [1 2 3 4] y [2 4 6 7] z [2 5 8]] (prn (* x y z)))


(comment
  "Streams are essentially like lazy procedures that do not compute till when they are needed
   
   It provided just the needed data at a given point in time and then the rest can be progressively gotten (streamed) when needed.")

(defn cons-stream [x y] (cons x (delay y)))

(defn head [s] (first s))

(defn tail [s] (force (rest s)))

;; The delay and force above are not magic. What delay does is just to create a
;; lambda that is suspended
;; That means when I call delay on a particular expression, I get a reference
;; to
;; a function that contains that expression
;; So what force just does is to call that expression when needed

(defn memo-proc
  [proc]
  (let [already-run? (atom nil)
        result (atom nil)]
    (fn []
      (if (not @already-run?)
        (do (reset! already-run? (not nil)) (reset! result (proc)))
        @result))))

(defn my-delay [& body] (memo-proc (fn [] body)))

(defn my-force [p] (p))

(def my-lazy-n (my-delay (do (print "Hello there") 10)))

(my-force my-lazy-n)

(range 10000)

(def r (map inc (range 10000)))

(realized? r)  ; => false (nothing computed yet)

(first r)      ; => 0 (computes first chunk)
(count (take 32 r))  ; => 32 (first chunk is already computed)
(nth r 33)

(realized? r)