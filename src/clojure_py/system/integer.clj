(ns clojure-py.system.integer
  (:require [clojure-py.constructors :refer :all]
            [clojure-py.object :as obj]))

(defc-struct Winteger-t
  :extends obj/object-t
  :members [:int :value])

(defc-gbl Winteger-type-t -> obj/type-t
  ["Integer"
   obj/standard-obj-free])

(defc-fn from-int [:int num -> obj/object*]
  (-> (c-new Winteger-t Winteger-type-t (const-int 1) num)
      (c-bitcast obj/object*)))

(defc-fn unwrap-int [obj/object* i
                     -> :int]
  (c-let [val (c-get i Winteger-t :value)]
         (obj/dec-ref i)
         val))