(ns clojure-py.pyast
  #_(:require #_[clojure-python.core :as cljpy]
            [clojure.data.json :as json])
  #_(:import [org.python.core PyObject PyList Py])
  (:require [clojure.java.shell :as sh]))

(comment
  (cljpy/init {:libpaths ["src/clojure_py/"]})

  (cljpy/py-import-lib ast)
  (cljpy/py-import-lib __builtin__)

  (cljpy/py-import-lib assembler)
  (def assemble (cljpy/py-fn assembler assemble))

  (defn ast-node [x]
    (let [f (.__finditem__ ast x)]
      (assert f (str "Could not find " x))
      (fn [& args]
        (cljpy/call f args))))


  (defn ast-node* [x]
    (let [f (.__finditem__ ast x)]
      (assert f (str "Could not find " x))
      (fn [args]
        (println args "baaaazzz")
        (let [l (PyList. args)]
          (println l "<-- LLLLL" f args)
          (println (into-array PyObject [l]) 0)
          (let [v (.__call__ f
                             (into-array PyObject [])
                             (into-array String []))]
            (.__setattr__ v "body" l)
            v)
          ))))

  (defn ast-value [x]
    (.__finditem__ ast x))

  (defn dump [a]
    (println a " --> " ((ast-node "dump") a))
    a)

  (defn assemble [s]
    (let [{:keys [out err]} (sh/sh "python3" "src/clojure_py/assembler.py" :in s
                                   :out "UTF-8"
                                   )]
      (println "Errors: " err)
      out))


  (defn compile [form]
    (let [r (assemble (json/write-str form))]
      (println r)
      (json/read-str r)))

  (defn run [form]
    (let [compiled (compile form)]
      (println compiled "<-- ")
      3)))