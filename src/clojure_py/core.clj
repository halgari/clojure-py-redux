(ns clojure-py.core
  (:require [cljs.analyzer :as an]
            [cljs.compiler :as faux] ; TODO: Remove the need for this
            [clojure-py.compiler :as comp]
            [clojure-py.core-macros :as macros]
            [clojure-py.pyast :as pyast]
            ))

(defn debug [x]
  (println x)
  x)


(defn analyze
  [form]
  (an/with-core-macros
    "cljs/core"
    (debug (an/analyze (an/empty-env) form))))

(defn compile
  [form]
  (debug (pyast/compile (debug (comp/to-ast (analyze form))))))

(defn run
  [form]
  (pyast/run  {:op "Module" :args* [{:op "Expression" :args [(comp/to-ast (analyze form))]}]}))