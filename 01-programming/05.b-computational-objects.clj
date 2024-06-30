(defn logical-not
  [s]
  (cond (= s 0) 1
        (= s 1) 0
        :else (throw (Exception. (str "Invalid signal: " s)))))
;; TODO:
(defn logical-and [s1 s2] (s1 s2))

(defn call-each [procedures & args] (doseq [proc procedures] (apply proc args)))
(defn make-wire
  []
  (let [signal (atom 0)
        action-procs (atom [])
        set-my-signal! (fn [new-value]
                         (cond (= signal new-value) 'done
                               :else (do (reset! signal new-value)
                                         (call-each action-procs))))
        accept-action-proc
          (fn [proc] (swap! action-procs (cons action-procs proc)) (proc))
        dispatch (fn [m]
                   (cond (= m 'get-signal) @signal
                         (= m 'set-signal!) set-my-signal!
                         (= m 'add-action!) accept-action-proc
                         :else (throw (Exception. (str "Bad message: " m)))))]
    dispatch))
(defn get-signal [wire] (wire 'get-signal))
(defn set-signal! [wire new-value] ((wire 'set-signal) new-value))
(defn add-action! [wire action] ((wire 'add-action!) action))


(def inverter-delay 2)
(def and-gate-delay 3)
(def or-gate-delay 3)

(def input-1 (make-wire))
(def input-2 (make-wire))
(def sum (make-wire))
(def carry (make-wire))

(def the-agenda (make-agenda))

(defn after-delay
  [delay action]
  (add-to-agenda! (+ delay (current-time the-agenda)) action the-agenda))
(defn propagate
  []
  (cond (empty? the-agenda) 'done
        :else (do ((first the-agenda))
                  (remove-first-item! the-agenda)
                  (propagate))))



(defn inverter
  [in out]
  (let [invert-in (fn []
                    (let [new-value (logical-not (get-signal in))]
                      (after-delay inverter-delay
                                   #(set-signal! out new-value))))]
    (add-action! in invert-in)))
(defn or-gate [a b c])
(defn and-gate
  [a1 a2 out]
  (let [and-action-proc
          (fn []
            (let [new-value (logical-and (get-signal a1) (get-signal a2))]
              (after-delay and-gate-delay #(set-signal! out new-value))))]
    (add-action! a1 and-action-proc)
    (add-action! a2 and-action-proc)))

(defn half-adder
  [a b s c]
  (let [d (make-wire)
        e (make-wire)]
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))

(defn full-adder
  [a b c-in sum c-out]
  (let [s (make-wire)
        c1 (make-wire)
        c2 (make-wire)]
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))

