(ns omnia.repl.store
  (:require [schema.core :as s]
            [omnia.repl.text :as t]
            [omnia.schema.text :refer [Text]]
            [omnia.schema.store :refer [Store SerialisedStore History LinearHistory EvalHistory Timeframe]]
            [omnia.util.arithmetic :refer [dec< inc<]]
            [omnia.util.misc :refer [slurp-or-else]]))

;; FIXME: Refactor. Make it less monolithic around stores

(s/defn create-linear-history :- LinearHistory
  [limit :- s/Int]
  {:timeframe '()
   :size      0
   :limit     limit})

(s/defn create-eval-history :- EvalHistory
  [limit :- s/Int]
  {:timeframe []
   :position  0
   :temp      nil
   :limit     limit})

(s/defn undo-history :- LinearHistory
  [store :- Store]
  (:undo-history store))

(s/defn redo-history :- LinearHistory
  [store :- Store]
  (:redo-history store))

(s/defn eval-history :- EvalHistory
  [store :- Store]
  (:eval-history store))

(s/defn timeframe :- Timeframe
  [history :- History]
  (:timeframe history))

(s/defn limit :- s/Int
  [history :- History]
  (:limit history))

(s/defn size :- s/Int
  [history :- LinearHistory]
  (:size history))

(s/defn position :- s/Int
  [history :- EvalHistory]
  (:position history))

(s/defn temp :- (s/maybe Text)
  [history :- EvalHistory]
  (:temp history))

(s/defn next-historical-value :- (s/maybe Text)
  [linear-history :- LinearHistory]
  (some-> linear-history (timeframe) (first)))

(s/defn prepend :- LinearHistory
  [text :- Text
   linear-history :- LinearHistory]
  (let [limit (limit linear-history)
        size  (size linear-history)
        frame (timeframe linear-history)]
    {:timeframe (if (< size limit)
                  (cons text frame)
                  (cons text (take (dec limit) frame)))
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

(s/defn append :- EvalHistory
  [text :- Text
   eval-history :- EvalHistory]
  (let [limit    (limit eval-history)
        temp     (temp eval-history)
        position (position eval-history)
        frame    (timeframe eval-history)
        size     (count frame)]
    {:timeframe (if (< size limit)
                  (conj frame text)
                  (conj (vec (drop 1 frame)) text))
     :position  (if (< position limit)
                  (inc position)
                  position)
     :limit     limit
     :temp      temp}))

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
   indexed :- EvalHistory]
  (assoc store :eval-history indexed))

(s/defn reset-eval-history :- Store
  [store :- Store]
  (let [history   (eval-history store)
        frame     (timeframe history)
        limit     (limit history)
        temp      (temp history)
        position  (position history)
        position' (min limit (count frame))
        temp'     nil]
    (if (and (= temp temp')
             (= position position'))
      store
      (with-eval-history store {:timeframe frame
                                :limit     limit
                                :position  position'
                                :temp      temp'}))))

(s/defn next-undo-value :- (s/maybe Text)
  [store :- Store]
  (-> store (undo-history) (next-historical-value)))

(s/defn next-redo-value :- (s/maybe Text)
  [store :- Store]
  (-> store (redo-history) (next-historical-value)))

(s/defn undo :- Store
  [store :- Store]
  (->> store (undo-history) (tail) (with-undo-history store)))

(s/defn redo :- Store
  [store :- Store]
  (->> store (redo-history) (tail) (with-redo-history store)))

(s/defn add-to-undo-history :- Store
  [store :- Store
   text :- Text]
  (->> store
       (undo-history)
       (prepend text)
       (with-undo-history store)))

(s/defn add-to-redo-history :- Store
  [store :- Store
   text :- Text]
  (->> store
       (redo-history)
       (prepend text)
       (with-redo-history store)))

(s/defn add-to-eval-history :- Store
  [store :- Store
   text :- Text]
  (->> store
       (eval-history)
       (append text)
       (with-eval-history store)))

(s/defn add-temporary :- Store
  [store :- Store
   text :- Text]
  (let [eval-history (eval-history store)
        position     (position eval-history)
        frame        (timeframe eval-history)
        limit        (limit eval-history)]
    (with-eval-history store {:timeframe frame
                              :position  position
                              :limit     limit
                              :temp      text})))

(s/defn travel-to-previous-position :- Store
  [store :- Store]
  (let [eval-history (eval-history store)
        position     (position eval-history)
        frame        (timeframe eval-history)
        limit        (limit eval-history)
        temp         (temp eval-history)
        position'    (dec position)]
    (with-eval-history store {:timeframe frame
                              :limit     limit
                              :temp      temp
                              :position  (if (> position' 0) position' 0)})))

(s/defn travel-to-next-position :- Store
  [store :- Store]
  (let [eval-history (eval-history store)
        position     (position eval-history)
        frame        (timeframe eval-history)
        limit        (limit eval-history)
        threshold    (min limit (count frame))
        temp         (temp eval-history)
        position'    (inc position)]
    (with-eval-history store {:timeframe frame
                              :limit     limit
                              :temp      temp
                              :position  (if (<= position' threshold) position' position)})))

(s/defn evaluation :- (s/maybe Text)
  [store :- Store]
  (let [history   (eval-history store)
        position  (position history)
        timeframe (timeframe history)
        temporary (temp history)]
    (nth timeframe position temporary)))

(s/defn create-store :- Store
  [limit :- s/Int]
  (let [linear (create-linear-history limit)
        eval   (create-eval-history limit)]
    {:undo-history linear
     :redo-history linear
     :eval-history eval}))

(s/defn read-store :- Store
  [path :- s/Str]
  (let [store (create-store 50)
        data  (-> path
                  (slurp-or-else "")
                  (:eval-history []))]
    (->> data
         (mapv t/from-string)
         (reduce add-to-eval-history store))))

(s/defn serialise :- SerialisedStore
  [store :- Store]
  {:eval-history (->> store (eval-history) (timeframe) (mapv t/as-string))})

(s/defn write-store :- nil
  [path :- s/Str
   store :- Store]
  (->> store (serialise) (spit path)))