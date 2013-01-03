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
         3)))
