(ns clojure-py.system.pointer-dict
  (:require [clojure-py.constructors :refer :all]
            [clojure-py.object :refer :all]))


(defstruct entry-t
  :members [:int :k
            object-t :v
            :i8* :next])

(def entry* (pointer entry-t))

(defstruct map-t
  :members [:int :count
            (c-pointer-t entry-t) :first])

(defn- key [local]
  (c-get local entry-t :k))

(defn- next-node [local]
  (c-get local entry-t :next))

(defn- new-node [k v]
  (-> (c-malloc entry-t)
      (c-set entry-t :k k)
      (c-set entry-t :v v)))

(defc-fn add-item [entry* map
                   :int k
                   object* val
                   -> entry*]
  (c-if (c-is (key map) k)
        (c-do (c-set map
                     entry-t
                     :val
                     val)
              val)
        (c-if (c-is (key next)
                    (nptr :i8*))
              (c-set map
                     entry-t
                     :next
                     (new-node k val))
              (c-recur (next-node map)
                       k
                       val))))

(defc-fn get-item [entry* map
                   :int k
                   object* else
                   -> object*]
  (c-if (c-is (key map) k)
        (val map)
        (c-if (c-is (next-node map) (nptr :i8*))
              else
              (c-recur (next-node map) k else))))