(defn find-average
  [numbers]
  (if (empty? numbers) nil (/ (reduce + numbers) (count numbers))))

(defn square [x] (* x x))

;; (defn improve-guess [guess x]
;;   (find-average [guess (/ x guess)]))

;; (defn good-enough? [guess x]
;;   (< (abs (- (square guess) x)) 0.001))


;; (defn try-guess [guess x]
;;   (if (good-enough? guess x) guess
;;       (try-guess (improve-guess guess x) x)))


(defn abs [x] (if (x < 0) (* -1 x) x))


(defn sqrt
  [x]
  (defn improve [guess] (find-average [guess (/ x guess)]))
  (defn good-enough? [guess] (< (abs (- (square guess) x)) 0.001))
  (defn try-guess
    [guess]
    (if (good-enough? guess) guess (try-guess (improve guess))))
  (try-guess 1))