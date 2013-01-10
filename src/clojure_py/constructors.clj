(ns clojure-py.constructors
  (:refer-clojure :as j #_:exclude #_[defstruct])
  (:require [clojure-py.llvmc :as llvmc]))

(defn c-struct [name options]
  (let [members (partition 2 (:members options))]
    {:type :struct
     :name name
     :extends (:extends options)
     :members (mapv first members)
     :names (mapv second members)}))

(defn make-accessor [type sym]
  (let [t (gensym)]
    `(defn ~(symbol (str "-" (name sym))) ~(vector t)
       (c-get ~t ~type ~sym))))

(defn d [x]
  (println x)
  x)

(defmacro defc-struct [name & options]
  (let [opts (apply hash-map options)]
    (d `(do
         (def ~name (c-struct ~(clojure.core/name name) ~opts))
         ~@(map (comp (partial make-accessor name) second)
                (partition 2 (:members opts)))))))


#_(defn c-fn
  [nm args & body]
  (let 
    (assert ret-type "Function must return a type (use -> type)")
    {:op :fn
     :type {:type :fn
            :args arg-types
            :ret ret-type}
     :name (name nm)
     :args (map second args)
     :body {:op :do
            :body body}
     :linkage (when (:extern (meta nm)) :external)}))

#_(defmacro defc-fn
  [name args & body]
  `(def ~name (c-fn (with-meta (symbol ~(clojure.core/name name))
                      ~(meta name))
                    ~args
                    ~@body)))


(defn c-do [& body]
  {:pre [(not (nil? body))]}
  {:op :do
   :body body})

(defn c-if [test then else]
  {:op :if
   :test test
   :then then
   :else else})

(defn c-is [a b]
  {:op :is
   :a a
   :b b})

(defn c-set [ptr tp member val]
  {:op :set
   :ptr ptr
   :member member
   :type tp
   :value val})

(defn c-get [ptr tp member]
  {:op :get
   :ptr ptr
   :member member
   :type tp})

(defn c-aget [ptr idx]
  {:op :aget
   :ptr ptr
   :idx idx})

(defn c-idec [val]
  {:op :isub
   :a val
   :b {:op :const
       :type :int
       :value 1}})

(defn c-iadd
  ([a b]
     {:op :iadd
      :a a
      :b b})
  ([a b & more]
     (reduce c-iadd
             (c-iadd a b)
             more)))

(defn c-shl [a bits]
  {:op :shl
   :a a
   :bits bits})

(defn c-shr [a bits]
  {:op :shr
   :a a
   :bits bits})

(defn c-and [a b]
  {:op :and
   :a a
   :b b})

(defn c-iinc [val]
  {:op :iadd
   :a val
   :b {:op :const
       :type :int
       :value 1}})

(defn c-call [fn & args]
  {:op :call
   :fn fn
   :args args})

(defn const-int [value]
  {:op :const
   :type :int
   :value value})

(defmacro c-let [bindings & body]
  (reduce (fn [a [local binding]]
            (let [s (name (gensym "let_"))]
              `{:op :let
                :local ~s
                :binding ~binding
                :body (let [~local {:op :local
                                    :name ~s}]
                        {:op :do
                         :body ~a})}))
          `{:op :do
            :body ~(vec body)}
          (reverse (partition 2 bindings))))

#_(defmacro c-local [name]
  `{:op :local
   :name ~(clojure.core/name name)})

(defn c-malloc [tp]
  {:op :malloc
   :type tp})

(defn c-recur [& args]
  {:op :recur
   :args args})

(defn gen-name [s]
  (name (gensym s)))

(defn c-pointer-t [tp]
  {:type :*
   :etype tp})

(defn c-nptr [tp]
  {:op :const
   :type tp
   :value nil})

(defn c-fn-t [args ret]
  {:pre [args ret]}
  {:type :fn
   :args args
   :ret ret})

(defmacro c-loc [name]
  {:op :local
   :name ~(clojure.core/name name)})

(defmacro c-fn [name tp args & body]
  {:pre [name tp args]}
  `{:op :fn
    :type ~tp
    :args ~(mapv clojure.core/name args)
    :name ~name
    :body (let ~(vec (mapcat (fn [x idx] [x {:op :arg
                                             :idx idx}])
                             args
                             (range)))
            (c-do ~@body))})

(defmacro defextern [name args ret]
  `(do (register-global (.getName ~'*ns*)
                        ~(clojure.core/name name)
                        {:op :fn
                         :linkage :external
                         :type {:type :fn
                                :args ~args
                                :ret ~ret}
                         :name ~(clojure.core/name name)})
       (def ~name {:op :get-global :name ~name})))

(def registered-globals (atom {}))

(defn register-global [ns nm gbl]
  (swap! registered-globals assoc-in [ns nm] gbl))

(defmacro defc-fn [name args & body]
  (let [args (partition 2 args)
       ret-fn (comp (partial = '->) first)
       ret-type (second (first (filter ret-fn args)))
       args (remove ret-fn args)
       args-map (zipmap (map second args)
                        (range))
        arg-types (mapv first args)]
    `(let [nsname# (.getName ~'*ns*)
           f# (c-fn (str nsname# "/" ~(clojure.core/name name))
                   (c-fn-t ~(mapv first args) ~ret-type)
                   ~(mapv second args)
                   ~@body)]
       (register-global nsname# ~(clojure.core/name name) f#)
       (defn ~name
         [& args#]
         {:op :call
          :fn {:op :get-global
               :name (:name f#)}
          :args (vec args#)}))))

(defn c-free [local]
  {:op :free
   :pointer local})

(defn c-bitcast [ptr tp]
  {:op :bitcast
   :value ptr
   :type tp})

(defn c-new [tp & inits]
  {:op :new
   :type tp
   :members (vec inits)})

(defn c-gbl [name tp data]
  {:op :global
   :name name
   :type tp
   :value data})

(defmacro defc-gbl [name _ tp  data]
  `(let [nsname# (.getName ~'*ns*)
         f# (c-gbl (str nsname# "/" ~(clojure.core/name name)) ~tp ~data)]
     (register-global nsname# ~(clojure.core/name name) f#)
     (def ~name {:op :get-global
                 :name (:name f#)})))


(defn c-module [includes & body]
  {:op :module
   :name "main"
   :body  (-> (reduce (fn [a x]
                        (concat a
                                (vals (@registered-globals x))))
                      []
                      includes)
              (concat body)
              vec)})
