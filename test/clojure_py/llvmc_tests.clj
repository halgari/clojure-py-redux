(ns clojure-py.llvmc-tests
  (:use clojure.test
        clojure-py.llvmc
        clojure.pprint))

(init-target)

(deftest llvm-type-test
  (is (llvm-type {:type :struct
                  :members [:long :int {:type :array
                                       :size 20
                                       :etype {:type :struct
                                               :members [:int :int :i8*]}}]}))
  (is (llvm-type {:type :fn
                   :ret :long
                   :args [:long {:type :array
                                 :size 20
                                 :etype :int}]})))

(deftest llvm-encode-consts
  (is (encode-const (llvm-type :int) 42))
  (is (encode-const (llvm-type {:type :struct
                                :members [:int :long]})
                    [42 43]))
  (is (encode-const (llvm-type {:type :struct
                                :members [:int {:type :array
                                                :etype :long
                                                :size 3}]})
                    [42 [1 2 3]]))
  (is (encode-const (llvm-type {:type :array
                                :size 1
                                :etype {:type :fn*
                                        :args []
                                        :ret :int}})
                    [nil]))
  (is (encode-const (llvm-type :i8*)
                    "footon"))
  (is (encode-const (llvm-type {:type :array
                                :size 1
                                :etype {:type :struct
                                        :members [:i8*]}})
                    [["footon"]])))


(pprint (target-seq))

(deftest llvm-compile-tests
  (is (compile {:op :module
                :name "module"
                :body [{:op :global
                        :type {:type :array
                               :size 3
                               :etype :int}
                        :name "SomeNumbers"
                        :value [42 43 44]}
                       {:op :fn
                        :type {:type :fn
                               :args []
                               :ret :int}
                        :name "foo_func"
                        :body {:op :const
                               :value 42
                               :type :int}}]}))
  (is
   (let [pyobj* :i8*
         pyfnc {:type :fn
                :args [:i8* :i8*]
                :ret :i8*}
         nil-const {:op :const
                    :value nil
                    :type :i8*}
         pyfnc-record {:type :struct
                       :members [:i8* {:type :*
                                       :etype pyfnc} :int :i8*]}
         py-init-module {:op :fn
                         :name "Py_InitModule4_64"
                         :linkage :extern
                         :type {:type :fn
                                :args [:i8* {:type :*
                                             :etype pyfnc-record}
                                       :i8* :i8*]
                                :ret :int}
                         }
         from_long {:op :fn
                    :type {:type :fn
                           :ret pyobj*
                           :args [:long]}
                    :linkage :extern
                    :name "PyInt_FromLong"}
         mod (compile {:op :module
                       :name "module"
                       :body (concat [from_long
                                      py-init-module]
                                     [{:op :fn
                                       :name "test_foo"
                                       :linkage :extern
                                       :type pyfnc
                                       :body {:op :call
                                              :fn "PyInt_FromLong"
                                              :args [{:op :const
                                                      :value 42
                                                      :type :long}]}}
                                      {:op :global
                                       :name "mbr_tbl"
                                       :type {:type :array
                                              :size 2
                                              :etype pyfnc-record}
                                       :value [["test_foo"
                                                {:global "test_foo"}
                                                 1
                                                 "Test Fnc"]
                                               [nil nil 0 nil]]}
                                       
                                      {:op :fn
                                       :name "initfoo"
                                       :linkage :extern
                                       :type {:type :fn
                                              :args []
                                              :ret :long}
                                       :body {:op :do
                                              :body [{:op :call
                                                      :fn "Py_InitModule4_64"
                                                      :args [{:op :const
                                                              :value "foo"
                                                              :type :i8*}
                                                             {:op :bitcast
                                                              :value
                                                              {:op :get-global
                                                               :name "mbr_tbl"}
                                                              :type {:type :*
                                                                     :etype pyfnc-record}}
                                                             nil-const
                                                             nil-const]}
                                                     {:op :const
                                                       :value 42
                                                       :type :long}]}}])
                       })]
     (link-object-file mod "foo.so" "x86-64" ["python-config" "--libs" "--ldflags"]))))