
(comment
  (do (def one-four (list 1 2 3 4))
      (println one-four)
      (first (rest (rest one-four))))
  (map inc '(1 2 3)))

(comment
  (do (defn coord-map [] ()) (println "")))


(comment
  (do (defn scale-list [s l] (map #(* % s) l)) (scale-list 3 [1 2 3 4])))

(comment
  "Implementing our own custom map function by just means of combination and recursion"
  (do
    (defn my-map
      [proc list]
      (if (seq list) (cons (proc (first list)) (my-map proc (rest list))) nil))
    (my-map #(* % 12) [1 2 3])))

(comment
  "Same as above, but for for-each"
  (do (defn my-for-each
        [proc list]
        (if (seq list)
          (do (proc (first list)) (my-for-each proc (rest list)))
          "done"))
      (my-for-each println [1 2 3])))

(comment
  "Same as above, but for filter"
  (do (defn my-filter
        [proc list]
        (if (seq list)
          (if (proc (first list))
            (cons (first list) (my-filter proc (rest list)))
            (my-filter proc (rest list)))
          nil))
      (my-filter odd? [1 2 3 5 6 7])))


(comment
  "Think about language as:
   
   What are the primitives?
   What are the means of combination?
   What are the means of abstraction?")
