(ns omnia.repl.store
  (:require [schema.core :as s]
            [omnia.schema.store :refer [Store History LinearHistory IndexedHistory Timeframe TimeEntry]]
            [omnia.util.arithmetic :refer [dec< inc<]]))

(s/defn timeframe :- Timeframe
  [history :- History]
  (:timeframe history))

(s/defn limit :- s/Int
  [history :- History]
  (:limit history))

(s/defn size :- s/Int
  [history :- LinearHistory]
  (:size history))

(s/defn instant :- s/Int
  [history :- IndexedHistory]
  (:instant history))

(s/defn prepend :- LinearHistory
  [entry :- TimeEntry
   linear-history :- LinearHistory]
  (let [limit (limit linear-history)
        size  (size linear-history)
        frame (timeframe linear-history)]
    {:timeframe (if (< size limit)
                  (cons entry frame)
                  (cons entry (take (dec limit) frame)))
     :size      (if (< size limit)
                  (inc size)
                  limit)
     :limit     limit}))

(s/defn tail :- LinearHistory
  [linear-history :- LinearHistory]
  (if (= 0 (size linear-history))
    linear-history
    {:timeframe (rest (timeframe linear-history))
     :size      (dec< (size linear-history) 0)
     :limit     (limit linear-history)}))

(s/defn append :- IndexedHistory
  [entry :- TimeEntry
   indexed :- IndexedHistory]
  (let [limit   (limit indexed)
        instant (instant indexed)
        frame   (timeframe indexed)
        size    (count frame)]
    {:timeframe (if (< size limit)
                  (conj frame entry)
                  (conj (vec (drop 1 frame)) entry))
     :instant   (cond
                  (= size 0) 0
                  (< (inc instant) limit) (inc instant)
                  :else instant)
     :limit     limit}))

(s/defn undo-history :- LinearHistory
  [store :- Store]
  (:undo-history store))

(s/defn redo-history :- LinearHistory
  [store :- Store]
  (:redo-history store))

(s/defn eval-history :- IndexedHistory
  [store :- Store]
  (:eval-history store))

(s/defn with-undo-history :- Store
  [store :- Store
   linear-history :- LinearHistory]
  (assoc store :undo-history linear-history))

(s/defn with-redo-history :- Store
  [store :- Store
   linear-history :- LinearHistory]
  (assoc store :redo-history linear-history))

(s/defn with-eval-history :- Store
  [store :- Store
   indexed :- IndexedHistory]
  (assoc store :eval-history indexed))

(s/defn undo :- Store
  [store :- Store]
  (let [undo (undo-history store)
        redo (redo-history store)]
    (-> store
        (with-undo-history (tail undo))
        (with-redo-history (prepend (first undo) redo)))))

(s/defn redo :- Store
  [store :- Store]
  (let [undo (undo-history store)
        redo (redo-history store)]
    (-> store
        (with-undo-history (prepend (first redo) undo))
        (with-redo-history (tail redo)))))

(s/defn add-to-undo-history :- Store
  [store :- Store
   entry :- TimeEntry]
  (->> store
       (undo-history)
       (prepend entry)
       (with-undo-history store)))

(s/defn add-to-eval-history :- Store
  [store :- Store
   entry :- TimeEntry]
  (->> store
       (eval-history)
       (append entry)
       (with-eval-history store)))

(s/defn travel-to-previous-instant :- Store
  [store :- Store]
  (let [eval-history (eval-history store)
        instant      (instant eval-history)
        frame        (timeframe eval-history)
        limit        (limit eval-history)]
    (with-eval-history store {:timeframe frame
                                :limit     limit
                                :instant   (dec< instant 0)})))

(s/defn travel-to-next-instant :- Store
  [store :- Store]
  (let [eval-history (eval-history store)
        instant      (instant eval-history)
        frame        (timeframe eval-history)
        limit        (limit eval-history)]
    (with-eval-history store {:timeframe frame
                                :limit     limit
                                :instant   (inc< instant limit)})))

(s/defn evaluation :- TimeEntry
  [store :- Store]
  (let [history     (eval-history store)
        instant   (instant history)
        timeframe (timeframe history)]
    (nth timeframe instant [])))

(s/defn create-linear-history :- LinearHistory
  [limit :- s/Int]
  {:timeframe '()
   :size      0
   :limit     limit})

(s/defn create-indexed-history :- IndexedHistory
  [limit :- s/Int]
  {:timeframe []
   :instant   0
   :limit     limit})

(s/defn create-store :- Store
  [limit :- s/Int]
  (let [linear-history (create-linear-history limit)
        indexed-history      (create-indexed-history limit)]
    {:undo-history linear-history
     :redo-history linear-history
     :eval-history indexed-history}))