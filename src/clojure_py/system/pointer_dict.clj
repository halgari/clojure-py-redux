(ns clojure-py.system.pointer-dict
  (:require [clojure-py.constructors :refer :all]
            [clojure-py.object :as obj]))


(defc-struct entry-t
  :members [:int :k
            obj/object* :v])

(def entry* (c-pointer-t entry-t))

(defc-struct pointer-dict-t
  :members [:int :count
            :int :size
            :int :mask
            entry* :table])

(def pointer-dict* (c-pointer-t pointer-dict-t))

(defc-fn new [-> pointer-dict*]
  (c-new pointer-dict-t 0 256 7 (-> (obj/calloc 16 256)
                                    (c-bitcast entry*))))

(defc-fn slot-free? [entry* table :int idx -> :bool]
  (c-is (-k (c-aget table idx)) (const-int 0)))

(defc-fn probe-1 [entry* table
                  :int idx
                  :int hash
                  :int perturb
                  :int mask
                  -> entry*]
  (c-let [i (c-iadd (c-shl idx (const-int 2))
                    idx
                    perturb
                    (const-int 1))]
         (c-let [newidx (c-and i mask)]
                (c-if (c-call slot-free? table idx)
                      (c-aget table idx)
                      (c-recur table newidx hash (c-shr perturb (const-int 5)) mask)))))

(defc-fn find-free [entry* table :int hash :int mask -> entry*]
  (c-let [idx (c-and hash mask)]
    (c-if (slot-free? table idx)
      (c-aget table idx)
      (probe-1 table idx hash hash mask))))

(defc-fn inc-table-count [pointer-dict* dict
                          -> pointer-dict*]
  ;; TODO: Resize full dicts
  (c-set dict pointer-dict-t :count (c-iadd (-count dict) (const-int 1))))


(defc-fn found-item? [entry* table :int idx :int k -> :i1]
  (c-is (-k (c-aget table idx)) k))

(defc-fn find-probe-1 [entry* table
                       :int idx
                       :int hash
                       :int perturb
                       :int mask
                       -> entry*]
  (c-let [i (c-iadd (c-shl idx (const-int 2))
                    idx
                    perturb
                    (const-int 1))
          newidx (c-and i mask)]
         (c-if (found-item? table newidx hash)
               (c-aget table newidx)
               (c-if (slot-free? table newidx)
                     (c-nptr entry*)
                     (c-recur table newidx hash (c-shr perturb (const-int 5)) mask)))))

(defc-fn add-item [pointer-dict* dict
                   :i8* ptr
                   obj/object* obj
                   -> pointer-dict*]
  (c-let [k (c-bitcast ptr :int)
        entry (find-free (-table dict) k (-mask dict))]
    (c-set entry entry-t :k k)
    (c-set entry entry-t :v obj)
    (inc-table-count dict)))



(defc-fn find-item [entry* table :int hash :int mask -> entry*]
  (c-let [idx (c-and hash mask)]
    (c-if (slot-free? table idx)
          (c-nptr entry*)
          (c-if (found-item? table idx hash)
                (c-aget table idx)
                (find-probe-1 table idx hash hash mask)))))

(defc-fn get-item [pointer-dict* dict
                   :i8* ptr
                   obj/object* default
                   -> obj/object*]
  (let [k (c-bitcast ptr :int)
        entry (find-item (-table dict) k (-mask dict))]))




