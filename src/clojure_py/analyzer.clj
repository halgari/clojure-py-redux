(ns clojure-py.analyzer)


(defn type->kw [form env]
  (cond (symbol? form) :symbol
        (integer? form) :int
        (seq? form) :seq))

(defmulti analyze-item type->kw)
(defmulti analyze-sexp ffirst)




(defmethod analyze-item :symbol
  [form env]
  "foo")
(defmethod analyze-item :int
  [form env]
  {:env env
   :op :const
   :type :int
   :value :int})

(declare analyze-native)
(defmethod analyze-item :seq
  [[f & args] env]
  (if (= (namespace f) "native")
    (analyze-native f args env))
  #_{:env env
   :op :call
   :fn (analyze-item f env)
   :args (map analyze-item args env)})


(defn analyze [form]
  (analyze-item form {}))