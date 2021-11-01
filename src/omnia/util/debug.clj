(ns omnia.util.debug)

(defmacro time-return [& body]
  `(let [s# (System/nanoTime)
         val# ~@body
         e# (System/nanoTime)
         total# (/ (- e# s#) 1000000.0)]
     [(str total# " ms") val#]))

(defmacro time-out [& body]
  `(let [[s# v#] (time-return ~@body)]
     (spit "debug" s#)
     v#))

(defmacro debug [body]
  `(spit "debug" (str ~body "\n") :append true))
