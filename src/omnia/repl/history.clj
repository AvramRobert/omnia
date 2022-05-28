(ns omnia.repl.history
  (:require [schema.core :as s]
            [omnia.schema.history :refer [History Epoch Timeframe TextInstant]]
            [omnia.util.arithmetic :refer [dec<]]))

(s/defn timeframe :- Timeframe
  [epoch :- Epoch]
  (:timeframe epoch))

(s/defn size :- s/Int
  [epoch :- Epoch]
  (:size epoch))

(s/defn limit :- s/Int
  [epoch :- Epoch]
  (:limit epoch))

(s/defn prepend :- Epoch
  [instant :- TextInstant
   epoch   :- Epoch]
  (let [limit (limit epoch)
        size  (size epoch)
        frame (timeframe epoch)]
    (if (< size limit)
      {:timeframe (cons instant frame)
       :size      (inc size)}
      {:timeframe (cons instant (take limit frame))
       :size      limit})))

(s/defn tail :- Epoch
  [epoch :- Epoch]
  (if (= 0 (size epoch))
    epoch
    {:timeframe (rest (timeframe epoch))
     :size      (dec< (size epoch) 0)}))

(s/defn undo-history :- Epoch
  [history :- History]
  (:undo-history history))

(s/defn redo-history :- Epoch
  [history :- History]
  (:redo-history history))

(s/defn with-undo-history :- History
  [history  :- History
   timeline :- Epoch]
  (assoc history :undo-history timeline))

(s/defn with-redo-history :- History
  [history  :- History
   timeline :- Epoch]
  (assoc history :redo-history timeline))

(s/defn undo :- History
  [history :- History]
  (let [undo (undo-history history)
        redo (redo-history history)]
    (-> history
        (with-undo-history (tail undo))
        (with-redo-history (prepend (first undo) redo)))))

(s/defn redo :- History
  [history :- History]
  (let [undo (undo-history history)
        redo (redo-history history)]
    (-> history
        (with-undo-history (prepend (first redo) undo))
        (with-redo-history (tail redo)))))

(s/defn add-to-history :- History
  [history :- History
   instant :- TextInstant]
  (->> history
       (undo-history)
       (prepend instant)
       (with-undo-history history)))

(s/defn create-epoch :- Epoch
  [limit :- s/Int]
  {:timeframe []
   :size      0
   :limit     limit})

(s/defn create-history :- History
  [limit :- s/Int]
  (let [epoch (create-epoch limit)]
    {:undo-history epoch
     :redo-history epoch}))