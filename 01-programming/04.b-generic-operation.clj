(defn square [x] (* x x))

(defn attach-type [type contents] [type contents])
(defn type' [datum] (first datum))
(defn contents [datum] (last datum))

(defn rectangular? [z] (= (type' z) 'rectangular))
(defn polar? [z] (= (type' z) 'polar))

(comment
  "complex number operations"
  (do (defn make-rectangular [x y] [x y])
      (defn real-part [z] (first z))
      (defn imag-part [z] (last z))
      (defn make-polar [r a] [(* r (Math/cos a)) (* r (Math/sin a))])
      (defn magnitude [z] (Math/sqrt (+ (square (first z)) (square (last z)))))
      (defn angle [z] (Math/atan (last z) (first z)))
      (defn +c
        [z1 z2]
        (make-rectangular (+ (real-part z1) (real-part z2))
                          (+ (imag-part z1) (imag-part z2))))
      (defn -c
        [z1 z2]
        (make-rectangular (- (real-part z1) (real-part z2))
                          (- (imag-part z1) (imag-part z2))))
      (defn *c
        [z1 z2]
        (make-polar (* (magnitude z1) (magnitude z2))
                    (+ (angle z1) (angle z2))))
      (defn ÷c
        [z1 z2]
        (make-polar (/ (magnitude z1) (magnitude z2))
                    (- (angle z1) (angle z2))))))

(comment
  "complex number operations with rectangular representation"
  (do (defn make-rectangular [x y] (attach-type 'rectangular [x y]))
      (defn real-part-rectangular [z] (first z))
      (defn imag-part-rectangular [z] (last z))
      (defn magnitude-rectangular
        [z]
        (Math/sqrt (+ (square (first z)) (square (last z)))))
      (defn angle-rectangular [z] (Math/atan (last z) (first z)))))

(comment
  "complex number operations with polar representation"
  (do (defn make-polar [r a] (attach-type 'polar [r a]))
      (defn real-part-polar [z] (* (first z) (Math/cos (last z))))
      (defn imag-part-polar [z] (* (first z) (Math/sin (last z))))
      (defn magnitude-polar [z] (first z))
      (defn angle-polar [z] (last z))))

(comment
  "a generic solution that handles both polar and rectangular representations"
  (do (defn real-part
        [z]
        (cond (rectangular? z) (real-part-rectangular (contents z))
              (polar? z) (real-part-polar (contents z))))
      (defn imag-part
        [z]
        (cond (rectangular? z) (imag-part-rectangular (contents z))
              (polar? z) (imag-part-polar (contents z))))
      (defn magnitude
        [z]
        (cond (rectangular? z) (magnitude-rectangular (contents z))
              (polar? z) (magnitude-polar (contents z))))
      (defn angle
        [z]
        (cond (rectangular? z) (angle-rectangular (contents z))
              (polar? z) (angle-polar (contents z)))))
  "This style uses a manager that is aware of the different implementations and dispatches accordingly")

;; Say we have this as the implementation table, which also has 2 methods one
;; for puting new implementations into the table,
;; and the other for retrieveing a particular implementation or a procedure in
;; an implementation
(def implementation-table
  (atom {'rectangular {'real-part real-part-rectangular,
                       'imag-part imag-part-rectangular,
                       'magnitude magnitude-rectangular,
                       'angle angle-rectangular},
         'polar {'real-part real-part-polar,
                 'imag-part imag-part-polar,
                 'magnitude magnitude-polar,
                 'angle angle-polar}}))

(defn get' [k1 k2 table] (get-in @table [k1 k2]))
(defn put' [k1 k2 value table] (reset! table (assoc-in @table [k1 k2] value)))

(comment
  "we could also use a generic solutions that just does an arbitrary lookup into a table filled by"
  "the different implementation designers (sort of like a spec)"
  "we could define an operate function"
  (do (defn operate
        [op obj]
        (let [proc (get' (type' obj) op implementation-table)]
          (if (not (nil? proc)) (proc (contents obj)) "some error")))
      (defn operate-2
        [op arg1 arg2]
        (if (= (type' arg1) (type' arg2))
          (let [proc (get' (type' arg1) op implementation-table)]
            (if (not (nil? proc))
              (proc (contents arg1) (contents arg2))
              "some error"))
          "args not the same"))
      (comment
        "here the obj is the table where the designers fill"
        "then we can use this to define the generic funnctions")
      (defn real-part [obj] (operate 'real-part obj))
      (defn imag-part [obj] (operate 'imag-part obj))
      (defn magnitude [obj] (operate 'magnitude obj))
      (defn angle [obj] (operate 'angle obj)))
  "This style is called data directed programming")


;; We could also create a table for generic things like addition, subtraction,
;; mulltiplication and division
;; by using the same systems we demonstrated above

(def table (atom {}))

;; We could do additions for primitive numbers with this system and add it to
;; our table
(defn make-number [n] (attach-type 'number n))
(defn +number [x y] (make-number (+ x y)))
(put' 'number 'add +number table)

;; We could do the same for complex numbers
(defn make-complex [z] (attach-type 'complex z))
(defn +complex [z1 z2] (make-complex (+c z1 z2)))
(put' 'complex 'add +complex table)

;; we can als do it for complex systems like polynomials
(defn make-polynomial
  [var terms-list]
  (attach-type 'polynomial (cons var terms-list)))
(defn +poly
  [p1 p2]
  (if (same-var? (var p1) (var p2))
    (make-polynomial (var p1) (+terms (term-list p1) (term-list p2)))
    "Error: poly not in the same var"))
(put' 'polynomial 'add +poly table)
