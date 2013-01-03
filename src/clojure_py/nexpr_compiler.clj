(ns clojure-py.nexpr-compiler)

(def ^:dynamic *locals*)

(defn type->kw [form]
  (cond (symbol? form) :symbol
        (integer? form) :int
        (seq? form) :seq))

(defmulti analyze-item type->kw)
(defmulti analyze-sexp #(first %))

(defmethod analyze-item :symbol
  [sym]
  (if-let [op (*locals* sym)] 
    op
    {:op :global
     :name (name sym)}))

(defmethod analyze-sexp :default
  [[nm & args]]
  {:op :call
   :fn {:op :global
        :name (name nm)}
   :args (mapv analyze-item args)})

(defmethod analyze-item :seq
  [itm]
  (analyze-sexp itm))

(defmethod analyze-item :int
  [itm]
  {:op :const
   :type :int
   :value itm})

(defmethod analyze-sexp 'float
  [[_ expr]]
  {:op :cast
   :type :float
   :body expr})

(defmethod analyze-sexp 'fdiv
  [[_ & ops]]
  (reduce
   (fn [a x]
     {:op :fdiv
      :div a
      :num x})
   (analyze-item (first ops))
   (next ops)))

(defmethod analyze-sexp 'aset
  [[_ arr idx v]]
  {:op :aset
   :idx (map analyze-item idx)
   :arr (analyze-item arr)
   :value (analyze-item v)})

(defmethod analyze-sexp 'aget
  [[_ arr idx]]
  (let [idx (if (vector? idx) idx [idx])]
    {:op :aget
     :idx (map analyze-item idx)
     :ptr (analyze-item arr)}))
 
(defmethod analyze-sexp 'fadd
  [[_ & ops]]
  (reduce
   (fn [a x]
     {:op :fadd
      :a a
      :b (analyze-item x)})
   (analyze-item (first ops))
   (next ops)))

(defmethod analyze-sexp 'fmul
  [[_ & ops]]
  (reduce
   (fn [a x]
     {:op :fmul
      :a a
      :b (analyze-item x)})
   (analyze-item (first ops))
   (next ops)))


(defn- inner-dotimes [[local max-val] body-fn]
  (assert (symbol? local) "Local must be a symbol")
  (let [sname (gensym (str (name local) "_max_"))]
    {:op :let
     :locals [[sname (analyze-item local)]]
     :body {:op :loop
      :locals [local]
      :inits [{:op :const
               :type :int
               :value 0}]
      :body {:op :do
             :body (binding [*locals* (assoc *locals* local)]
                     [(body-fn)
                      {:op :if
                       :test {:op :eq
                              :a {:op :local :name local}
                              :b {:op :local :name sname}}
                       :then {:op :const
                              :type :int
                              :value 0}
                       :else {:op :recur
                              :locals [{:op :iadd
                                        :a {:op :local
                                            :name local}
                                        :b {:op :const
                                            :type :int
                                            :value 1}}]}}])}}}))

(defn- outer-dotimes [b body]
  (if b
    (inner-dotimes (first b)
                   #(outer-dotimes (next b) body))
    (analyze-sexp (list 'do body))))

(defmethod analyze-sexp 'dotimes
  [[_ b & body]]
  (let [binds (partition 2 b)]
    (outer-dotimes binds body)))

(defmethod analyze-sexp 'nfn
  [[_ nm args & body]]
  (let [args (partition 2 args)
        ret-fn (comp (partial = '->) first)
        ret-type (second (first (filter ret-fn args)))
        args (remove ret-fn args)
        args-map (zipmap (map second args)
                         (range))
        arg-types (mapv first args)]
    (assert ret-type "Function must return a type (use -> type)")
    {:op :fn
     :type {:type :fn
            :args arg-types
            :ret ret-type}
     :name (name nm)
     :args (map second args)
     :body (when body
             {:op :do
                      :body (binding
                                 [*locals* 
                                  (zipmap (map last args)
                                          (map (fn [idx]
                                                 {:op :arg
                                                  :idx idx})
                                               (range)))]
                               (mapv analyze-item
                                     body))})
     :linkage (when (:extern (meta nm)) :external)}))

(defn- parse-fn [name args body])

(defn debug [x]
  (println x)
  x)

(defmacro def-nfn
  [name args & body]
  (debug `(def ~name  (analyze-item ~(list 'quote (list* 'nfn name args body))))))

(defmacro native-code
  [& body]
  (debug `(map analyze-item ~(list 'quote body))))
