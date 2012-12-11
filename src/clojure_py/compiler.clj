(ns clojure-py.compiler)

(def third (comp second next))

(def to-ast)

(defmulti -to-ast :op)
(defmulti -native #(-> % :form second))

(defmethod -to-ast :js
  [form]
  (-native form))

(defmethod -to-ast :constant
  [{:keys [op env form]}]
  (cond (integer? form) {:op "Num" :args [form]}
        :else (assert false (str "Unknown form type" (type form)))))

(defmethod -native "binop"
  [{:keys [children form]}]
  (let [op (third form)]
    {:op "BinOp"
     :args [(-to-ast (second children))
            {:op op}
            (-to-ast (third children))]}))

(defn to-ast [form]
  (let [f (-to-ast form)
        env (:env form)]
    (assoc f
      :line (:line env)
           :column (:column env))))
