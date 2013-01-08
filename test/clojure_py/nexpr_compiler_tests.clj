(ns clojure-py.nexpr-compiler-tests
  (:use clojure.test
        clojure-py.nexpr-compiler
        clojure.pprint)
  (:require [clojure-py.llvmc :as llvmc]))

(deftest testing-basic-exprs
  (is (native-code (nfn ^:extern clock [-> :int]))))

(deftest build-simple-exe
  (is (= (-> (native-code (nfn main [:int argc :i8** argv -> :int]
                               42))
             (debug)
             (llvmc/compile-as-exe)
             (llvmc/run-exe)
             :exit)
         42))
  (is (= (-> (native-code (nfn ^:extern strlen [:i8* str -> :int])
                          (nfn main [:int argc :i8** argv -> :int]
                               (strlen (aget argv 1))))
             (debug)
             (llvmc/compile-as-exe)
             (llvmc/run-exe "foo")
             :exit)
         3))
  (is (= (let [rgb-t {:type :struct
                      :members [:int :int :int]
                      :names [:r :g :b]}]
           (-> (native-code (nfn main [:int argc :i8** argv -> :int]
                                 (let [rgb (alloc rgb-t)]
                                   (set! rgb rgb-t :r 4)
                                   (set! rgb rgb-t :g 6)
                                   (set! rgb rgb-t :b 9)
                                   (iadd (get rgb rgb-t :r)
                                         (get rgb rgb-t :g)
                                         (get rgb rgb-t :b)))))
               (debug)
               (llvmc/compile-as-exe)
               (llvmc/run-exe)
               :exit))
         19)))
