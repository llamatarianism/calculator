(defn operator? [s]
  (re-find #"\(?[\*\+-/]" s))

(defn precedence [s]
  (cond
    (re-find #"\*|/" s) 1
    (re-find #"\+|-" s) 0
    :else -1)) ; it's not an operator.

(defn find-place [op stack]
  (loop [new-stack stack output []]
    (if (or (empty? new-stack) (>= (precedence op) (precedence (peek stack))))
      [(conj new-stack op) output]
      (recur (pop new-stack) (conj output (peek new-stack))))))

(defn shunting-yard [s]
  (let [expr (rest (clojure.string/split s #"(?<!\d)|(?!\d)"))]
    (loop [expr expr stack [] output []]
      (println expr)
      (println stack)
      (println output)
      (cond
        ;; Base case.
        (empty? expr)
        (into output (reverse stack))
        ;; It's an operator.
        (operator? (first expr))
        (let [[new-stack new-output] (find-place (first expr) stack)]
          (recur (rest expr) new-stack (into output new-output)))
        ;; It's a number.
        :else
        (recur (rest expr) stack (conj output (first expr)))))))

(defn parse-postfix [expr]
  (loop [expr expr stack []]
    (println expr)
    (println stack)
    (cond
      ;; Base case
      (empty? expr)
      (peek stack)
      ;; It's an operator.
      (operator? (first expr))
      (recur
       (rest expr)
       (conj
        (pop (pop stack))
        (str
         ((eval (symbol (first expr)))
          (read-string (peek (pop stack)))
          (read-string (peek stack))))))
      ;; It's a number.
      :else
      (recur (rest expr) (conj stack (first expr))))))

(def calculate (comp parse-postfix shunting-yard))

(calculate (clojure.string/trim (read-line)))
