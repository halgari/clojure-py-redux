(ns clojure-py.llvmc
  (:import (com.sun.jna Native Pointer Memory))
  (:require [clojure.java.shell :as shell]
            [clojure.string :as string]))

(def ^:dynamic *lib* 'LLVM-3.1)


(defn get-function [s]
  `(com.sun.jna.Function/getFunction ~(name *lib*) ~(name s)))

(defn debug [s]
  (println s)

  s)

(def debug-mode false)

(defmacro defnative
  [return-type function-symbol]
  `(let [func# ~(get-function function-symbol)]
     (defn ~(symbol (name function-symbol))
       [& args#]
       (let [r# (.invoke func# ~return-type (to-array args#))]
         (when debug-mode
           (println "After " ~(name function-symbol))
           (System/gc)
           (System/runFinalization)
           (Thread/sleep 500))
         r#))))

(defn new-pointer []
  (let [p (Memory. Pointer/SIZE)]
   (.clear p)
    p))


(defn to-pointers [& args]
  (let [arr (make-array Pointer (count args))]
    (loop [a args
           c 0]
      (if a
        (do (aset arr c (first a))
            (recur (next a) (inc c)))
        arr))))


(def LLVMCCallConv 0)
(def LLVMFastCallConv 8)
(def LLVMColdCallConv 9)
(def LLVMX86StdcallCallConv 64)
(def LLVMX86FastcallCallConv 65)
(defnative Integer LLVMSetFunctionCallConv)
(defnative Integer LLVMFindFunction)

(defnative Pointer LLVMAppendBasicBlock)
(defnative Pointer LLVMCreateBuilder)

(defnative Pointer LLVMGetParam)

(defnative Integer LLVMLinkInJIT)
'(defnative Integer LLVMInitializeNativeTarget)

(defnative Pointer LLVMModuleCreateWithName)

(defnative Pointer LLVMInt32Type)
(defnative Pointer LLVMFunctionType)

(defnative Pointer LLVMAddFunction)

(defnative Integer LLVMPositionBuilderAtEnd)

(defnative Boolean LLVMVerifyModule)

(def LLVMAbortProcessAction 0)
(def LLVMPrintMessageAction 1)
(def LLVMReturnStatusAction 2)

(defnative Pointer LLVMCreateModuleProviderForExistingModule)

(defnative Integer LLVMDisposeMessage)
(defnative Integer LLVMCreateJITCompiler)
(defnative Integer LLVMCreateInterpreterForModule)
(defnative Pointer LLVMCreatePassManager)
(defnative Pointer LLVMGetExecutionEngineTargetData)
(defnative Integer LLVMAddTargetData)
(defnative Integer LLVMRunPassManager)
(defnative Integer LLVMDumpModule)
(defnative Integer LLVMDisposePassManager)
(defnative Integer LLVMDisposeExecutionEngine)
(defnative Integer LLVMBuildRet)

(defnative Integer LLVMLinkInJIT)
(defnative Integer LLVMLinkInInterpreter)
(defnative Integer LLVMInitializeX86Target)
(defnative Integer LLVMInitializeX86TargetInfo)
(defnative Integer LLVMInitializeX86TargetMC)
(defnative Pointer LLVMRunFunction)
(defnative Boolean LLVMFindFunction)
(defnative Pointer LLVMCreateGenericValueOfInt)
(defnative Integer LLVMGenericValueToInt)
(defnative Pointer LLVMBuildAdd)
(defnative Pointer LLVMBuildSub)
(defnative Pointer LLVMConstInt)
(defnative Pointer LLVMBuildICmp)
(defnative Pointer LLVMIntType)
(defnative Pointer LLVMBuildCondBr)
(defnative Pointer LLVMBuildPhi)
(defnative Integer LLVMAddIncoming)
(defnative Pointer LLVMTypeOf)
(defnative Integer LLVMCountParamTypes)
(defnative Integer LLVMGetTypeKind)
(defnative Integer LLVMDisposeGenericValue)
(defnative Integer LLVMDisposeBuilder)
(defnative Pointer LLVMBuildBr)
(defnative Pointer LLVMBuildCall)
(defnative Pointer LLVMBuildAlloca)
(defnative Pointer LLVMBuildFree)
(defnative Pointer LLVMBuildLoad)
(defnative Pointer LLVMBuildStore)

(defnative Integer LLVMAddConstantPropagationPass)
(defnative Integer LLVMAddInstructionCombiningPass)
(defnative Integer LLVMAddPromoteMemoryToRegisterPass)
(defnative Integer LLVMAddGVNPass)
(defnative Integer LLVMAddCFGSimplificationPass)
(defnative Pointer LLVMBuildArrayMalloc)
(defnative Pointer LLVMBuildGEP)
(defnative Pointer LLVMBuildBitCast)
(defnative Pointer LLVMConstString)
(defnative Pointer LLVMConstInt)
(defnative Integer LLVMCountStructElementTypes)
(defnative Pointer LLVMConstPointerCast)
(defnative Pointer LLVMGetStructElementTypes)
(defnative Integer LLVMGetTypeKind)
(defnative Pointer LLVMConstPointerNull)
(defnative Pointer LLVMInt64Type)
(defnative Pointer LLVMStructType)
(defnative Pointer LLVMArrayType)
(defnative Pointer LLVMDumpValue)
(defnative Integer LLVMGetArrayLength)
(defnative Pointer LLVMGetElementType)
(defnative Pointer LLVMConstArray)
(defnative Pointer LLVMConstString)
(defnative Pointer LLVMConstStruct)
(defnative Pointer LLVMConstGEP)
(defnative Pointer LLVMConstBitCast)
(defnative Integer LLVMCountParams)
(defnative Pointer LLVMAddGlobal)
(defnative Integer LLVMSetInitializer)
(defnative Integer LLVMWriteBitcodeToFile)
(defnative Pointer LLVMGetNamedGlobal)
(defnative Pointer LLVMGetNamedFunction)
(defnative Pointer LLVMInt8Type)
(defnative Pointer LLVMPointerType)
(defnative Integer LLVMSetLinkage)
(defnative Integer LLVMGetIntTypeWidth)
(defnative Pointer LLVMBuildStructGEP)
(defnative Pointer LLVMBuildAdd)
(defnative Pointer LLVMBuildSub)
(defnative Pointer LLVMBuildMalloc)

(def ^:dynamic *module* (LLVMModuleCreateWithName "tmp"))
(def ^:dynamic *fn*)
(def ^:dynamic *locals*)
(def ^:dynamic *builder*)
(def ^:dynamic *block*)


(def LLVMIntEQ 32)

(defmacro defenum
  [nm defs]
  (list* 'do
        `(def ~nm {:idx ~(zipmap (range)
                                   (map (comp keyword name) defs))
                     :defs ~(zipmap (map (comp keyword name) defs)
                                    (range))})
        (map-indexed (fn [idx d]
                       `(def ~d ~idx))
                     defs)))

(defenum LLVMTypeKind
  [LLVMVoidTypeKind
   LLVMHalfTypeKind
   LLVMFloatTypeKind
   LLVMDoubleTypeKind
   LLVMX86_FP80TypeKind
   LLVMFP128TypeKind
   LLVMPPC_FP128TypeKind
   LLVMLabelTypeKind
   LLVMIntegerTypeKind
   LLVMFunctionTypeKind
   LLVMStructTypeKind
   LLVMArrayTypeKind
   LLVMPointerTypeKind
   LLVMVectorTypeKind
   LLVMMetadataTypeKind
   LLVMX86_MMXTypeKind])

(defenum LLVMCodeGentFileType
  [LLVMAssemblyFile
   LLVMObjectFile])

(defenum LLVMRelocMode
  [LLVMRelocDefault
   LLVMRelocStatic
   LLVMRelocPIC
   LLVMRelocDynamicNoPIC])

(defenum LLVMCodeGenOptLevel
  [LLVMCodeGenLevelNone
   LLVMCodeGenLevelLess
   LLVMCodeGenLevelDefault
   LLVMCodeGenLevelAggressive])

(defenum LLVMCodeModel
  [LLVMCodeModelDefault
   LLVMCodeModelJITDefault
   LLVMCodeModelSmall
   LLVMCodeModelKernel
   LLVMCodeModelMedium
   LLVMCodeModelLarge])


(defenum LLVMLinkage
  [LLVMExternalLinkage,    ; Externally visible function 
   LLVMAvailableExternallyLinkage,
   LLVMLinkOnceAnyLinkage, ; Keep one copy of function when linking (inline)
   LLVMLinkOnceODRLinkage, ; Same, but only replaced by something equivalent. 
   LLVMWeakAnyLinkage,     ; Keep one copy of function when linking (weak) 
   LLVMWeakODRLinkage,     ; Same, but only replaced by something equivalent. 
   LLVMAppendingLinkage,   ; Special purpose, only applies to global arrays 
   LLVMInternalLinkage,    ; Rename collisions when linking (static functions)
   LLVMPrivateLinkage,     ; Like Internal, but omit from symbol table 
   LLVMDLLImportLinkage,   ; Function to be imported from DLL 
   LLVMDLLExportLinkage,   ; Function to be accessible from DLL 
   LLVMExternalWeakLinkage,; ExternalWeak linkage description 
   LLVMGhostLinkage,       ; Obsolete 
   LLVMCommonLinkage,      ; Tentative definitions 
   LLVMLinkerPrivateLinkage, ; Like Private, but linker removes. 
   LLVMLinkerPrivateWeakLinkage, ; Like LinkerPrivate, but is weak. 
   LLVMLinkerPrivateWeakDefAutoLinkage]) ; Like LinkerPrivateWeak, but possibly hidden. 


(defn init-target []
  (LLVMLinkInJIT)
  (LLVMLinkInInterpreter)
  (LLVMInitializeX86TargetInfo)
  (LLVMInitializeX86Target)
  (LLVMInitializeX86TargetMC))

(defn map-parr [fn coll]
  (into-array Pointer
              (map fn coll)))

(def kw->linkage
  {:extern LLVMExternalLinkage})

(declare llvm-type
         )

(defmulti llvm-type-to-data (fn [tp]
                              (get-in LLVMTypeKind [:idx (LLVMGetTypeKind tp)])))

(defmethod llvm-type-to-data :LLVMPointerTypeKind
  [tp]
  {:type :*
   :etype (llvm-type-to-data (LLVMGetElementType tp))})

(defmethod llvm-type-to-data :LLVMIntegerTypeKind
  [tp]
  {:type :int
   :width (LLVMGetIntTypeWidth tp)})

(defmethod llvm-type-to-data :LLVMArrayTypeKind
  [tp]
  {:type :array
   :etype (llvm-type-to-data (LLVMGetElementType tp))
   :size (LLVMGetArrayLength tp)})

(defmethod llvm-type-to-data :LLVMStructTypeKind
  [tp]
  (let [cnt (LLVMCountStructElementTypes tp)
        arr (make-array Pointer cnt)]
    (LLVMGetStructElementTypes tp arr)
    {:type :struct
     :members (mapv llvm-type-to-data arr)}))

(defmethod llvm-type-to-data :LLVMFunctionTypeKind
  [tp]
  :fn*)

(defmulti encode-const (fn [tp v]
                         (get-in LLVMTypeKind [:idx (LLVMGetTypeKind tp)])))

(defn const-string-array [s]
  (let [ar (into-array Pointer (map #(LLVMConstInt (llvm-type :i8) % false)
                                    (concat s [0])))
        llvm-ar (LLVMConstArray (llvm-type :i8)
                        ar
                        (count ar))
        idx (into-array Pointer
                        [(LLVMConstInt (llvm-type :int) 0)])
        gbl (LLVMAddGlobal *module* (llvm-type {:type :array
                                                :size (count ar)
                                                :etype :i8})
                           (name (gensym "str_")))
        casted (LLVMConstBitCast gbl
                                 (llvm-type :i8*))]
    (LLVMSetInitializer gbl llvm-ar)

    casted
    ))

(defmethod encode-const :LLVMPointerTypeKind
  [tp v]
  (cond
   (map? v) (LLVMGetNamedFunction *module* (:name v))
   (string? v) (const-string-array v) #_(LLVMConstString v (count v) false)
   (instance? Pointer v) v
   (nil? v) (LLVMConstPointerNull tp)
   :else (assert false (str "Can't create pointer from " v))))

(defmethod encode-const :LLVMFunctionTypeKind
  [tp v]
  (assert (or (string? v)
              (nil? v)))
  (println "=========" tp (LLVMGetTypeKind tp))
  (if (nil? v)
    (LLVMConstPointerNull tp)
    (let [fnc (LLVMGetNamedFunction *module* v)]
      (assert fnc (str "Couldn't find " v))
      fnc)))

(defmethod encode-const :LLVMIntegerTypeKind
  [tp v]
  (LLVMConstInt tp v true))

(defmethod encode-const :LLVMArrayTypeKind
  [tp v]
  (println (llvm-type-to-data tp))
  (let [alen (LLVMGetArrayLength tp)
        atp (LLVMGetElementType tp)
        els (into-array Pointer
                                    (debug (map encode-const
                                                (repeat atp)
                                                v)))]
    (assert (= alen (count v)) (str "Wrong number of elements to constant array" alen " got " (count v)))
    (println "---------------------- " v)
    (LLVMConstArray atp
                    els
                    alen)))

(defmethod encode-const :LLVMStructTypeKind
  [tp v]
  (let [cnt (LLVMCountStructElementTypes tp)
        arr (make-array Pointer cnt)]
    (assert (= cnt (count v)))
    (LLVMGetStructElementTypes tp arr)
    (LLVMConstStruct (into-array Pointer
                                 (debug (map encode-const arr v)))
                     cnt
                     false)))


(println "Init LLVM")

(defprotocol ILLVMTypeDesc
  (llvm-type [this]))


(defmulti -llvm-type-kw identity)

(defmethod -llvm-type-kw :default
  [kw]
  (if (= (last (name kw)) \*)
    (LLVMPointerType (->> (butlast (name kw))
                          (apply str)
                          keyword
                          llvm-type)
                     0)
    (assert false (str "Unknown type " kw))))


(defmethod -llvm-type-kw :int
  [kw]
  (LLVMIntType 32))

(defmethod -llvm-type-kw :long
  [kw]
  (LLVMIntType 64))

(defmethod -llvm-type-kw :i8
  [kw]
  (LLVMInt8Type))

(defmethod -llvm-type-kw :i8*
  [kw]
  (llvm-type
   {:type :*
    :etype :i8}))


(defn flatten-struct [tp attr]
  (->> (take-while (complement nil?)
                   (iterate :extends tp))
       reverse
       (mapcat attr)))

(defn seq-idx [col itm]
  {:post [%]}
  (-> (zipmap col
              (range))
      (get itm)))

(defmulti -llvm-type-assoc :type)
(defmethod -llvm-type-assoc :struct
  [{:keys [members packed] :as struct}]
  (assert members)
  (let [ele (into-array Pointer
                        (map llvm-type
                             (flatten-struct struct :members)))
        packed (or packed false)]
    (LLVMStructType ele (count ele) packed)))


(defmethod -llvm-type-assoc :*
  [{:keys [etype]}]
  (LLVMPointerType (llvm-type etype) 0))

(defmethod -llvm-type-assoc :array
  [{:keys [size etype]}]
  (assert (and size etype))
  (LLVMArrayType (llvm-type etype) size))

(defmethod -llvm-type-assoc :fn
  [{:keys [ret args vararg?]}]
  (LLVMFunctionType (llvm-type ret)
                    (into-array Pointer (map llvm-type args))
                    (count args)
                    (or vararg? false)))

(defmethod -llvm-type-assoc :fn*
  [mp]
  (LLVMPointerType (-llvm-type-assoc (assoc mp :type :fn)) 0))



(extend-protocol ILLVMTypeDesc
  clojure.lang.Keyword
  (llvm-type [this]
    (-llvm-type-kw this))
  clojure.lang.Associative
  (llvm-type [this]
    (-llvm-type-assoc this)))

(defn nstruct [name types & opts]
  (let [opts (apply hash-map opts)]))

(def genname (comp name gensym))



(defn value-at [ptr]
  (.getPointer ptr 0))



(defmulti stub-global :op)

(defmethod stub-global :global
  [{:keys [name type linkage]}]
  (assert (and name type))
  (let [tp (llvm-type type)
        gbl (LLVMAddGlobal *module* tp name)]
    (when linkage
      (LLVMSetLinkage gbl (kw->linkage linkage)))
    gbl))

(defmethod stub-global :fn
  [{:keys [name type linkage]}]
  (println "stub" name)
  (let [tp (llvm-type type)
        gbl (LLVMAddFunction *module* name tp)]
    (when linkage
      (LLVMSetLinkage gbl (kw->linkage linkage)))
    gbl))

(defn op [o]
  (println (:op o))
  (:op o))

(defmulti compile op)

(defmethod compile :const
  [{:keys [value type]}]
  (let [tp (llvm-type type)]
    (encode-const tp value)))

(defmethod compile :global
  [{:keys [type value name]}]
  {:pre [type value name]}
  (let [val (LLVMGetNamedGlobal *module* name)]
    (println "========== init ==========" name type)
    (LLVMSetInitializer val (encode-const (llvm-type type) value))))

(defmethod compile :aget
  [{:keys [idx ptr]}]
  (assert (and idx ptr))
  (println "gep" (count idx) idx (map-parr compile idx))
  (let [gep (LLVMBuildGEP *builder*
                          (compile ptr)
                          (map-parr compile idx)
                          (count idx)
                          (name (gensym "aget_")))]
    (println "g")
    (LLVMBuildLoad *builder* gep "load_")))

(defmethod compile :call
  [{:keys [fn args]}]
  (let [fnc (compile fn)]
    (assert fnc (str "Couldn't find function " fn))
    (LLVMBuildCall *builder*
                   fnc
                   (map-parr compile args)
                   (count args)
                   (genname "call_"))))

(defmethod compile :do
  [{:keys [body]}]
  {:pre [(seq? (seq body))]}
  (doseq [x (butlast body)]
    (compile x))
  (compile (last body)))

(defmethod compile :let
  [{:keys [local binding body]}]
  (clojure.core/binding [*locals* (assoc *locals* local (compile binding))]
    (compile body)))

(defmethod compile :new
  [{:keys [type members]}]
  (let [malloc (LLVMBuildMalloc *builder* (llvm-type type) (name (gensym "new_")))]
    (doseq [idx (range (count members))]
      (let [gep (LLVMBuildStructGEP *builder*
                                    malloc
                                    idx
                                    (name (gensym "gep_")))]
        (LLVMBuildStore *builder* (compile (nth members idx)) gep)))
    malloc))

(defmethod compile :get
  [{:keys [ptr member type]}]
  (let [idx (seq-idx (flatten-struct type :names) member)
        _ (assert idx)
        cptr (compile {:op :bitcast
                       :value ptr
                       :type {:type :*
                              :etype type}})
        _ (println type idx)
        gep (LLVMBuildStructGEP *builder*
                                cptr
                            idx
                            (name (gensym "get_")))]
    (LLVMBuildLoad *builder* gep "load_")))

(defmethod compile :set
  [{:keys [ptr member type value]}]
  (let [idx (seq-idx (flatten-struct type :names) member)
        ptr (compile {:op :bitcast
                      :value ptr
                      :type {:type :*
                             :etype type}})
        gep (LLVMBuildStructGEP *builder* ptr idx (name (gensym "set_")))]
    (LLVMBuildStore *builder* (compile value) gep)
    ptr))


(defmethod compile :isub
  [{:keys [a b]}]
  (LLVMBuildSub *builder*
                (compile a)
                (compile b)
                (name (gensym "isub_"))))

(defmethod compile :if
  [{:keys [test then else]}]
  (let [thenblk (LLVMAppendBasicBlock *fn* (name (gensym "then_")))
        elseblk (LLVMAppendBasicBlock *fn* (name (gensym "else_")))
        endblk (LLVMAppendBasicBlock *fn* (name (gensym "end_")))
        cmpval (compile test)
        _ (LLVMPositionBuilderAtEnd *builder* thenblk)
        thenval (binding [*block* thenblk]
                  (compile then))
        _ (LLVMPositionBuilderAtEnd *builder* elseblk)
        elseval (binding [*block* elseblk]
                  (compile else))
        _ (LLVMPositionBuilderAtEnd *builder* *block*)
        tmp (LLVMBuildAlloca *builder* (LLVMTypeOf thenval) "alloca_")]
    (LLVMBuildCondBr *builder* cmpval thenblk elseblk)
    (LLVMPositionBuilderAtEnd *builder* thenblk)
    (LLVMBuildStore *builder* thenval tmp)
    (LLVMBuildBr *builder* endblk)
    (LLVMPositionBuilderAtEnd *builder* elseblk)
    (LLVMBuildStore *builder* elseval tmp)
    (LLVMBuildBr *builder* endblk)
    (LLVMPositionBuilderAtEnd *builder* endblk)
    (LLVMBuildLoad *builder* tmp (name (gensym "ifval_")))))

(defmethod compile :iadd
  [{:keys [a b]}]
  (LLVMBuildAdd *builder*
                (compile a)
                (compile b)
                (name (gensym "iadd_"))))

(defmethod compile :free
  [{:keys [pointer]}]
  (LLVMBuildFree *builder* (compile pointer))
  (compile {:op :const :type :int :value 0}))


(defmethod compile :get-global
  [{:keys [name]}]
  {:pre [name]}
  (println name)
  (or
   (LLVMGetNamedGlobal *module* name)
   (LLVMGetNamedFunction *module* name)))

(defmethod compile :local
  [{:keys [name]}]
  {:post [%]}
  (println *locals*, name)
  (*locals* name))

(defmethod compile :bitcast
  [{:keys [type value]}]
  {:pre [type value]}
  (LLVMBuildBitCast *builder*
                    (compile value)
                    (llvm-type type)
                    (name (gensym "bitcast_"))))

(defmethod compile :is
  [{:keys [a b]}]
  (LLVMBuildICmp *builder*
                 LLVMIntEQ
                 (compile a)
                 (compile b)
                 (name (gensym "is_"))))

(defmethod compile :arg
  [{:keys [idx]}]
  (LLVMGetParam *fn* idx))

(defmethod compile :fn
  [{:keys [type args name body]}]
  (when body
    (let [fnc (LLVMGetNamedFunction *module* name)
          pcnt (LLVMCountParams fnc)
          newargs (into {} (map (fn [s idx]
                                  [s (LLVMGetParam fnc idx )])
                                args
                                (range pcnt)))]
      (LLVMSetFunctionCallConv fnc LLVMCCallConv)
      (binding [*fn* fnc
                *locals* newargs
                *block* (LLVMAppendBasicBlock fnc (genname "fblk_"))]
        (LLVMPositionBuilderAtEnd *builder* *block*)
        (LLVMBuildRet *builder* (compile body) (genname "return_"))
        fnc))))

(defmethod compile :module
  [{:keys [body name]}]
  (let [error (new-pointer)
        module (LLVMModuleCreateWithName name )]
    (binding [*module* module
              *builder* (LLVMCreateBuilder)]
      (doseq [x body]
        (stub-global x))
      (doseq [x body]
        (println (:op x) "<______")
        (compile x))
      (LLVMVerifyModule module LLVMAbortProcessAction error)
      (LLVMDumpModule module)
      (LLVMDisposeMessage (value-at error))
      module)))

(defmethod compile :default
  [ast]
  (assert false (str "Can't compile" ast)))

(defn temp-file [prefix ext]
  (let [file (java.io.File/createTempFile prefix ext)]
    (.deleteOnExit file)
    (.getCanonicalPath file)))

(defn dump-module-to-temp-file [module]
  (let [file (temp-file "mod_dump" ".bc")]
    (LLVMWriteBitcodeToFile module file)
    file))


(defn write-object-file [module march] 
  (let [file (dump-module-to-temp-file module)
        ofile (temp-file "o_dump" ".o")
        cmds ["llc" "-filetype=obj" "-o" ofile file]
        cmds (if march (concat cmds ["--march" march]) cmds)
        {:keys [out err exit] :as mp} (apply shell/sh cmds)]
    (apply shell/sh ["llc" "-filetype=asm" "-o" "foo.s" file])
    (println cmds)
    (assert (= exit 0) err)
    
    ofile))

(defn interpret-opt [op]
  (cond (vector? op)
        (let [res (apply shell/sh op)]
          (assert (= 0 (:exit res)) (:err res))
          (string/split (string/trim (:out res)) #"[ \n]"))
        :else
        [op]))

(defn link-object-file [module filename march & opts]
  (let [tmp (write-object-file module march)
        opts (mapcat interpret-opt opts)
        cmds (concat ["gcc" tmp]
                                    opts
                                    ["-o" filename "--shared"])
        _ (println cmds)
        res (apply shell/sh cmds)]
    (assert (= 0 (:exit res)) res)
    (:out res)))

(defn link-exe [obj out]
  (let [cmds (concat ["gcc" obj "-o" out "-lc"])
        _ (println cmds)
        res (apply shell/sh cmds)]
    (assert (= 0 (:exit res)) res)
    (:out res)))



(defn compile-as-exe [ast]
  (let [mod (compile ast)
        ofile (write-object-file mod "x86-64")
        exe-file (temp-file "exe_gen" "out")
        out (link-exe ofile exe-file)]
    exe-file))

(defn run-exe [file & args]
     (apply shell/sh file args))


;;;;; TargetMachine Code ;;;;


(defnative Pointer LLVMGetFirstTarget)
(defnative Pointer LLVMGetNextTarget)
(defnative String LLVMGetTargetName)
(defnative String LLVMGetTargetDescription)
(defnative Boolean LLVMTargetHasJIT)
(defnative Boolean LLVMTargetHasTargetMachine)
(defnative Boolean LLVMTargetHasAsmBackend)
(defnative String LLVMGetTarget)
(defnative Pointer LLVMCreateTargetMachine)
(defnative Boolean LLVMTargetMachineEmitToFile)
(defnative Pointer LLVMGetTargetMachineData)

(defn target-info [t]
  {:target t
   :name (LLVMGetTargetName t)
   :desc (LLVMGetTargetDescription t)
   :jit? (LLVMTargetHasJIT t)
   :machine? (LLVMTargetHasTargetMachine t)
   :asm? (LLVMTargetHasAsmBackend t)})

(defn target-seq
  ([]
     (let [ft (LLVMGetFirstTarget)]
       (when ft
         (cons (target-info ft)
               (lazy-seq
                (target-seq ft))))))
  ([t]
     (let [nt (LLVMGetNextTarget t)]
       (when nt
         (cons (target-info nt)
               (lazy-seq
                (target-seq nt)))))))

(defn make-target-machine [module]
  (let [target (LLVMGetTarget module)]
    (println "--->" target)
    (LLVMCreateTargetMachine (:target
                              (first (target-seq)))
                             "x86_64-apple-darwin12.2.0"
                             "i686"
                             ""
                             LLVMCodeGenLevelDefault
                             LLVMRelocPIC
                             LLVMCodeModelDefault)))

(defn emit-to-file [module filename]
  (let [target (make-target-machine module)
        err (new-pointer)
        pass (LLVMCreatePassManager)]
    (LLVMAddTargetData (LLVMGetTargetMachineData target) pass)
    (LLVMRunPassManager pass module)
    
    (when (LLVMTargetMachineEmitToFile target module filename LLVMObjectFile err)
      (assert false (.getString (value-at err) 0)))
    (LLVMDisposeMessage (value-at err))
    (LLVMDisposePassManager pass)))