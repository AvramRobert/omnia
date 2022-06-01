(ns omnia.repl.store
  (:require [schema.core :as s]
            [omnia.repl.text :as t]
            [omnia.schema.text :refer [Text]]
            [omnia.schema.store :refer [Store SerialisedStore History UndoRedoHistory EvalHistory Timeframe]]
            [omnia.util.arithmetic :refer [dec< inc<]]
            [omnia.util.misc :refer [slurp-or-else]]))

;; FIXME: Refactor. Make it less monolithic around stores

(s/defn undo-history :- UndoRedoHistory
  [store :- Store]
  (:undo-history store))

(s/defn redo-history :- UndoRedoHistory
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
  [history :- UndoRedoHistory]
  (:size history))

(s/defn instant :- s/Int
  [history :- EvalHistory]
  (:instant history))

(s/defn temp :- (s/maybe Text)
  [history :- EvalHistory]
  (:temp history))

(s/defn with-undo-history :- Store
  [store :- Store
   linear-history :- UndoRedoHistory]
  (assoc store :undo-history linear-history))

(s/defn with-redo-history :- Store
  [store :- Store
   linear-history :- UndoRedoHistory]
  (assoc store :redo-history linear-history))

(s/defn with-eval-history :- Store
  [store :- Store
   indexed :- EvalHistory]
  (assoc store :eval-history indexed))

(s/defn reset-eval-history :- Store
  [store :- Store]
  (let [history  (eval-history store)
        frame    (timeframe history)
        limit    (limit history)
        temp     (temp history)
        instant  (instant history)
        instant' (min limit (count frame))
        temp'    nil]
    (if (and (= temp temp')
             (= instant instant'))
      store
      (with-eval-history store {:timeframe frame
                                :limit     limit
                                :instant   instant'
                                :temp      temp'}))))

(s/defn prepend :- UndoRedoHistory
  [text :- Text
   linear-history :- UndoRedoHistory]
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

(s/defn tail :- UndoRedoHistory
  [linear-history :- UndoRedoHistory]
  (if (= 0 (size linear-history))
    linear-history
    {:timeframe (rest (timeframe linear-history))
     :size      (dec< (size linear-history) 0)
     :limit     (limit linear-history)}))

(s/defn append :- EvalHistory
  [text :- Text
   eval-history :- EvalHistory]
  (let [limit   (limit eval-history)
        temp    (temp eval-history)
        instant (instant eval-history)
        frame   (timeframe eval-history)
        size    (count frame)]
    {:timeframe (if (< size limit)
                  (conj frame text)
                  (conj (vec (drop 1 frame)) text))
     :instant   (if (< instant limit)
                  (inc instant)
                  instant)
     :limit     limit
     :temp      temp}))

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
   text :- Text]
  (->> store
       (undo-history)
       (prepend text)
       (with-undo-history store)))

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
        instant      (instant eval-history)
        frame        (timeframe eval-history)
        limit        (limit eval-history)]
    (with-eval-history store {:timeframe frame
                              :instant   instant
                              :limit     limit
                              :temp      text})))

(s/defn travel-to-previous-instant :- Store
  [store :- Store]
  (let [eval-history (eval-history store)
        instant      (instant eval-history)
        frame        (timeframe eval-history)
        limit        (limit eval-history)
        temp         (temp eval-history)
        instant'     (dec instant)]
    (with-eval-history store {:timeframe frame
                              :limit     limit
                              :temp      temp
                              :instant   (if (> instant' 0) instant' 0)})))

(s/defn travel-to-next-instant :- Store
  [store :- Store]
  (let [eval-history (eval-history store)
        instant      (instant eval-history)
        frame        (timeframe eval-history)
        limit        (limit eval-history)
        threshold    (min limit (count frame))
        temp         (temp eval-history)
        instant'     (inc instant)]
    (with-eval-history store {:timeframe frame
                              :limit     limit
                              :temp      temp
                              :instant   (if (<= instant' threshold) instant' instant)})))

(s/defn evaluation :- (s/maybe Text)
  [store :- Store]
  (let [history   (eval-history store)
        instant   (instant history)
        timeframe (timeframe history)
        temporary (temp history)]
    (nth timeframe instant temporary)))

(s/defn create-undo-redo-history :- UndoRedoHistory
  [limit :- s/Int]
  {:timeframe '()
   :size      0
   :limit     limit})

(s/defn create-eval-history :- EvalHistory
  [limit :- s/Int]
  {:timeframe []
   :instant   0
   :temp      nil
   :limit     limit})

(s/defn create-store :- Store
  [limit :- s/Int]
  (let [undo-redo (create-undo-redo-history limit)
        eval      (create-eval-history limit)]
    {:undo-history undo-redo
     :redo-history undo-redo
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