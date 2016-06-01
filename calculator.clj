(defn operator?
  "Returns true if the input is '*', '/', '+' or '-'."
  [s]
  (boolean (re-find #"\(?[\*\+-/]" s)))

(defn precedence
  "Finds the precedence of an operator. '*' and '/' are higher than '+' and '-'."
  [s]
  (cond
    (re-find #"\*|/" s) 1
    (re-find #"\+|-" s) 0
    :else (throw (IllegalArgumentException.
                  "Argument to `predence` must be an `operator?`"))))

(defn find-place
  "Takes in an operator and the current stack.
   Returns the new stack and anything that should be popped."
  [op stack]
  (loop [new-stack stack output []]
    ;; Pop while the stack isn't empty,
    ;; and while the operator at the top has a lower precedence than the operator to insert.
    (if (or (empty? new-stack) (>= (precedence op) (precedence (peek stack))))
      [(conj new-stack op) output]
      (recur (pop new-stack) (conj output (peek new-stack))))))

(defn shunting-yard
  "Algorithm to convert infix expression into postfix."
  [s]
  ;; Split the string wherever the character before or after isn't a number.
  ;; e.g. 11+2-3 -> ["" "11" "+" "2" "-" "3"]
  (let [expr (rest (clojure.string/split s #"(?<!\d)|(?!\d)"))]
    (loop [expr expr stack [] output []]
      (cond
        ;;; Base case.
        (empty? expr)
        ;; Add all the remaining operators to the output.
        (into output (reverse stack)) 
        ;;; It's an operator.
        (operator? (first expr))
        ;; Find where to add the operator.
        (let [[new-stack new-output] (find-place (first expr) stack)]
          ;; Add all popped elements to the output queue.
          (recur (rest expr) new-stack (into output new-output))) 
        ;;; It's a number.
        :else
        ;; Put it into the output queue.
        (recur (rest expr) stack (conj output (first expr)))))))

(defn eval-simple-postfix
  "Evaluates a very simple postfix expression which contains only 2 operators and an operand."
  [oprs opns1 opns2]
  ((eval (symbol oprs)) (read-string opns1) (read-string opns2)))

(defn eval-complex-postfix
  "Evaluates a postfix expression."
  [expr]
  (loop [expr expr stack []]
    (cond
      ;;; Base case.
      (empty? expr)
      ;; Return the last element on the stack, which is the value of the expression.
      (peek stack)
      ;;; It's an operator.
      (operator? (first expr))
      (recur
       (rest expr)
       (conj
        ;; Pop 2 elements.
        (pop (pop stack))
        ;; Push the result of some operation with the 2 elements removed.
        (str (eval-simple-postfix (first expr) (peek (pop stack)) (peek stack)))))
      ;;; It's a number.
      :else
      (recur (rest expr) (conj stack (first expr))))))  ; Push it onto the stack.

;;; Infix expression -> Postfix expression -> String -> Integer
(def calculate (comp read-string eval-complex-postfix shunting-yard))

(calculate (clojure.string/trim (read-line)))
