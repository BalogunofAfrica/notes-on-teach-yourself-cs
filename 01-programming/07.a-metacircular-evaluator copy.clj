(defn lookup [a b] (a b))
(defn evlist [a b] (a b))
(defn primitive? [a] a)

(defn my-eval
  [expr env]
  (cond (number? expr) expr
        (symbol? expr) (lookup expr env)
        (= (first expr) 'quote) (second expr)
        (= (first expr) 'lambda) (list 'closure (last expr) env)
        (= (first expr) 'cond) (cond (last expr) env)
        :else (apply (my-eval (first expr) env) (evlist (last expr) env))))

(def env {'a 1})

(my-eval 'a env)

;; (defn my-apply [proc args]
;;   (cond (primitive? proc) (apply-primorp proc args)
;;         (= (first proc) 'closure) ()
;;         :else 1))


(apply max [1 20 3])