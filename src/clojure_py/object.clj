(ns clojure-py.object
  (:refer-clojure :exclude [defstruct])
  (:use [clojure-py.constructors]))

(def dealloc-fn-t
  {:type :fn
   :args [:i8*]
   :ret :int})


(defc-struct type-t
  :members [:i8* :Soname
            dealloc-fn-t :dealloc-fn])

(defc-struct object-t
  :members [:int :ob-type
            :int :ref-cnt])


(def object* (c-pointer-t object-t))

(defc-fn standard-obj-free [object* o -> :int]
  (c-free o)
  (const-int 0))


(defn ref-cnt [local]
  (c-get local object-t :ref-cnt))

(defn ob-type [local]
  (c-get local object-t :ob-type))

(defn dealloc-fn [tp]
  (c-get tp type-t :dealloc-fn))

(defn dec-ref [local]
  (c-do (c-set local object-t :ref-cnt
              (c-idec (ref-cnt local)))
       (c-if (c-is (ref-cnt local)
                   (const-int 0))
             (c-do
              (c-call (dealloc-fn (ob-type local)) local)
              (const-int 0))
             (ref-cnt local))))

(defn inc-ref [local]
  (c-let [nm (ref-cnt local)]
         (c-set local object-t :ref-cnt
                (c-iinc (c-local nm)))
         (c-local nm)))

(defn inc-refed [local]
  (c-do (inc-ref local)
        local))

(defn dec-refed [local]
  (c-do (dec-ref local)
        local))


