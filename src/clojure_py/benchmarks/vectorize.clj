(ns clojure-py.benchmarks.vectorize)
(comment

  (def clock-t :size_t)

  (def-nfn ^:extern clock [])
  (def-nfn ^:extern rand [:int max_val])

  (def matrix-size 1000)

  (def-nfn rand-float [-> float]
    (fdiv (float (rand 10000)) 10000))

  (def matrix-f-t {:type :array
                   :etype {:type :array
                           :etype :float
                           :size matrix-size}
                   :size matrix-size})

  (def-nfn matrix-mul [matrix-f-t a
                       matrix-f-t b
                       matrix-f-t c
                       -> int]
    (dotimes [i matrix-size
              j matrix-size
              k matrix-size]
      (aset c [i,j]
            (+ (aget c [i j])
               (* (aget a [i j])
                  (aget b [j k]))))))

  (def-nfn init-matrix [matrix-f-t a
                        -> :void]
    (dotimes [i matrix-size
              j matrix-size]
      (aset a [i j] (rand-float))))

  (def-nfn zero-matrix [matrix-f-t a
                        -> :void]
    (dotimes [i matrix-size
              j matrix-size]
      (aset a [i j] 0)))

  (def-nfn benchmark-matrix-f [-> :void]
    (dotimes [i 10]
      (let [a (malloc matrix-f-t)
            b (malloc matrix-f-t)
            c (malloc matrix-f t)]
        (init-matrix a)
        (init-matrix b)
        (zero-matrix c)
        (matrix-mul a b c)
        (free a b c))))

  (def-nfn main [:int argc
                 :i8** argv
                 -> :int]
    (benchmark-matrix-f)
    0))