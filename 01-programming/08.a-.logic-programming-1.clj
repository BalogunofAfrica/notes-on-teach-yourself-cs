
(comment
  "Programming is a way of communicating ideas")

(defn -merge
  [x y]
  (cond (empty? x) y
        (empty? y) x
        :else (let [a (first x)
                    b (first y)]
                (if (< a b)
                  (cons a (-merge (rest x) y))
                  (cons b (-merge x (rest y)))))))


(comment
  "We are trying to encode relations rather than programs as things that takes inputs and outputs. 
   We can mix/match our logic to find intermmidiate answers that would not be obvious when we are just dealing with them as just a blackbox that takes input and gives some output"
  "With relations our programs may have more than one answer"
  "We us logic to:
   1. Check what is true
   2. Express what is true
   3. Find out what is true")


(defn lisp-value [pred x y] (pred x y))

(lisp-value > 1 2)

(-merge [1 3] [2 7])

(comment
  "Building a logic language we have to consider what are the 
   1. Primitives: This would be a query which we can use to test what is true
   2. Means of combination: This would be operations like AND, NOT , OR etc.. with which we can combine premitives to find out what is true
   3. Means if abstraction: This would be like setting a rule to create other queries. We essentially express what is true by defining the body of the rule.")