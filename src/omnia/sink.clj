(ns omnia.sink
  (:import (java.util.concurrent Executors ExecutorService Future LinkedBlockingQueue)
           (clojure.lang IDeref IBlockingDeref)))

;; An agent-like thing with a single thread executor
;; Suitable for linearising function executions
(deftype Sink [queue executor task state errors]
  IDeref
  (deref [_] (deref state))
  IBlockingDeref
  (deref [_ timeout else] (deref state timeout else)))

(defn- attempt [f s]
  (try [(f s) nil]
    (catch Exception e
      [s e])))

(defn- queue-consumer [^LinkedBlockingQueue q a-state a-errors]
  (reify Runnable
    (run [_]
      (loop [state @a-state]
        (let [f  (.take q)
              g  (fn [err] #(conj % err))
              [ns err] (attempt f state)
              _  (some->> err g (swap! a-errors))
              _  (reset! a-state ns)]
          (recur ns))))))

(defn sink [s]
  (let [s  (atom s)
        er (atom [])
        q  ^LinkedBlockingQueue (LinkedBlockingQueue.)
        e  ^ExecutorService (Executors/newSingleThreadExecutor)
        t  (.submit e (queue-consumer q s er))]
    (Sink. q e t s er)))

(defn dispatch! [sink f]
  (let [queue (.queue sink)
        _     (.add queue f)]
    sink))

(defn kill! [sink]
  (let [t (.task sink)
        e (.executor sink)]
    (.cancel ^Future t true)
    (.shutdown ^ExecutorService e)))

(defn errors [sink]
  @(.errors sink))

(defn error [sink]
  (-> sink (.errors) (deref) (last)))