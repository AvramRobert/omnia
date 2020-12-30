(ns omnia.test-utils
  (:require [clojure.test :refer [is]]
            [omnia.hud :refer [Hud]]
            [omnia.context :refer [Context HighlightType]]
            [omnia.input :refer [Seeker]]
            [omnia.event :refer [Event InputEvent]]
            [omnia.more :refer [-- Region do-gen]]
            [omnia.config :as c]
            [omnia.input :as i]
            [omnia.hud :as h]
            [omnia.context :as r]
            [omnia.nrepl :as server]
            [omnia.terminal :as t]
            [omnia.event :as e]
            [schema.core :as s]
            [clojure.test.check.generators :as gen])
  (:import (java.util UUID)))

(defn one [generator] (rand-nth (gen/sample generator)))

(defn many
  ([generator] (many generator (rand-int 100)))
  ([generator n] (vec (repeatedly n #(one generator)))))

(defmacro <=>seeker [this-seeker that-seeker]
  `(is (= (:lines ~this-seeker) (:lines ~that-seeker))
       (str "Failed for inputs: \n" ~this-seeker " :: \n" ~that-seeker)))

(defmacro <=>hud [this-hud that-hud]
  `(<=>seeker (:seeker ~this-hud) (:seeker ~that-hud)))

(defmacro can-be [val & fs]
  `(do ~@(map (fn [f#] `(is (~f# ~val) (str "Failed for input: \n" ~val))) fs)))

(defn random-uuid []
  (str (UUID/randomUUID)))

(defn nrepl-result [map]
  (assoc map :id (random-uuid)
             :session (random-uuid)
             :ns "ns"
             :status ["done"]))

(defn rand-cursor [seeker]
  (let [y (-> seeker (:height) (rand-int))
        x (-> seeker (i/reset-y y) (i/line) (count) (rand-int))]
    [x y]))

(def gen-line
  (->> gen/char-alphanumeric
       (gen/vector)
       (gen/such-that (comp not empty?))))

(def gen-text
  (->> gen-line
       (gen/vector)
       (gen/such-that (comp not empty?))))

(def gen-seeker
  (->> gen-text
       (gen/fmap i/seeker)
       (gen/fmap #(i/move % (fn [_] (rand-cursor %))))))

(defn gen-seeker-of [size]
  (->> (gen/vector gen-line size)
       (gen/fmap i/seeker)
       (gen/fmap #(i/move % (fn [_] (rand-cursor %))))))

(defn gen-completion [size]
  (letfn [(candidate [s]
            {:candidate s
             :ns        ""
             :type      ""})]
    (->> (gen/vector (gen/not-empty gen/string-alphanumeric) size)
         (gen/fmap (fn [ss] {:completions (map candidate ss)})))))

(defn gen-evaluation [string]
  (gen/return {:value string}))

(defn gen-nrepl-result [gen-response]
  (do-gen [id       gen/uuid
           session  gen/uuid
           ns       gen/string-alphanumeric
           response gen-response]
    (-> response
        (assoc :id (str id)
               :session (str session)
               :ns ns
               :status ["done"])
        (list))))

(defn test-terminal [{:keys [refresh!
                             clear!
                             size
                             move!
                             put!
                             stop!
                             start!
                             get-event!]
                      :as   fns}]
  (assert (map? fns) "The input to `test-terminal` should be a map (look at omnia.test-utils)")
  (let [unit (constantly nil)]
    {:clear!     (or clear! unit)
     :refresh!   (or refresh! unit)
     :size       (or size (constantly 10))
     :move!      (or move! unit)
     :put!       (or put! unit)
     :stop!      (or stop! unit)
     :start!     (or start! unit)
     :get-event! (or get-event! unit)}))

;; FIXME: provide a seeker generator for :seeker
(defn gen-context [{:keys [size fov seeker receive history]
                    :or   {size    0
                           fov     10
                           receive (gen/return {})
                           seeker  i/empty-seeker
                           history []}}]
  (do-gen [hud-seeker (gen-seeker-of size)
           response   (gen-nrepl-result receive)]
    (-> (r/context (c/convert c/default-config)
                   (test-terminal {:size (constantly fov)})
                   (server/client {:host    ""
                                   :port    0
                                   :history history
                                   :client  (constantly response)}))
        (r/with-text seeker)
        (r/with-hud (h/hud hud-seeker fov)))))

(def up (e/event e/up))
(def down (e/event e/down))
(def left (e/event e/left))
(def right (e/event e/right))
(def select-all (e/event e/select-all))
(def select-down (e/event e/select-down))
(def select-up (e/event e/select-up))
(def select-right (e/event e/select-right))
(def select-left (e/event e/select-left))
(def expand (e/event e/expand))
(def copy (e/event e/copy))
(def paste (e/event e/paste))
(def backspace (e/event e/backspace))
(def enter (e/event e/break))
(def undo (e/event e/undo))
(def redo (e/event e/redo))
(def scroll-up (e/event e/scroll-up))
(def scroll-down (e/event e/scroll-down))
(defn character [k] (e/event e/character k))
(def clear (e/event e/clear))
(def evaluate (e/event e/evaluate))
(def prev-eval (e/event e/prev-eval))
(def next-eval (e/event e/next-eval))
(def parens-match (e/event e/match))
(def suggest (e/event e/suggest))
(def ignore (e/event e/ignore))
(def backspace (e/event e/backspace))
(def refresh (e/event e/refresh))

(s/defn process :- Context
  [ctx :- Context, events :- [Event]]
  (reduce (comp :ctx r/process) ctx events))

(s/defn process' :- Seeker
  [seeker :- Seeker, events :- [InputEvent]]
  (reduce i/process seeker events))

(s/defn process-one :- Context
  ([ctx :- Context event :- Event]
   (process-one ctx event 1))
  ([ctx   :- Context
    event :- Event
    n     :- s/Int]
   (->> (range 0 n)
        (reduce (fn [nctx _] (-> nctx (r/process event) (:ctx))) ctx))))

(s/defn overview :- s/Int
  [ctx :- Context]
  (-> ctx (r/preview-hud) (h/overview)))

(s/defn scroll-offset :- s/Int
  [ctx :- Context]
  (-> ctx (r/preview-hud) (h/scroll-offset)))

(s/defn field-of-view :- s/Int
  [ctx :- Context]
  (-> ctx (r/preview-hud) (h/field-of-view)))

(s/defn project-y :- s/Int
  [ctx :- Context]
  (let [view  (r/preview-hud ctx)
        [_ y] (-> view (h/text) (:cursor))]
    (h/project-y view y)))

(s/defn project-preview :- Seeker
  [ctx :- Context]
  (h/project-hud (r/preview-hud ctx)))

(defn project-cursor [ctx]
  (-> ctx (r/preview-hud) (h/project-hud-cursor)))

(defn cursor [ctx]
  (-> ctx (r/preview-hud) (h/text) (:cursor)))

(defn suggestions [ctx]
  (-> ctx (r/client) (server/complete! i/empty-seeker) (server/result)))

(defn server-history [ctx]
  (-> ctx (r/client) (:history)))

(defn highlights? [highlited region]
  (let [{expected-start :start
         expected-end   :end} region
        {actual-start :start
         actual-end   :end} (:region highlited)]
    (and (= expected-start actual-start)
         (= expected-end actual-end))))

(s/defn no-projection :- Region
  [ctx :- Context]
  (let [preview (r/preview-hud ctx)]
    {:start [0 (h/bottom-y preview)]
     :end   [0 (h/bottom-y preview)]}))

(s/defn project-highlight :- Region
  [ctx :- Context, h-key :- HighlightType]
  (let [preview   (r/preview-hud ctx)
        selection (-> ctx (r/highlights) (get h-key) (:region))]
    (h/clip-selection preview selection)))

(s/defn pop-up :- Hud
  [ctx :- Context, window :- Hud]
  (-> ctx (r/preview-hud) (h/pop-up window)))

(s/defn at-input-start :- Context
  [ctx :- Context]
  (let [text (-> ctx (r/input-area) (i/start))]
    (-> ctx (r/with-text text) (r/refresh))))

(s/defn at-input-end :- Context
  [ctx :- Context]
  (let [text (-> ctx (r/input-area) (i/end))]
    (-> ctx (r/with-text text) (r/refresh))))

(s/defn at-line-start :- Context
  [ctx :- Context]
  (let [text (-> ctx (r/input-area) (i/start-x))]
    (-> ctx (r/with-text text) (r/refresh))))

(s/defn at-line-end :- Context
  [ctx :- Context]
  (let [text (-> ctx (r/input-area) (i/end-x))]
    (-> ctx (r/with-text text) (r/refresh))))

(s/defn at-view-top :- Context
  [ctx :- Context]
  (let [fov       (-> ctx (r/preview-hud) (h/field-of-view))
        top-line #(-- % (dec fov)) ;; (dec) because we want to land on the fov'th line
        text      (-> ctx (r/input-area) (i/move-y top-line))]
    (-> ctx (r/with-text text) (r/refresh))))

(s/defn at-view-bottom :- Context
  [ctx :- Context]
  (let [fov         (-> ctx (r/preview-hud) (h/field-of-view))
        bottom-line #(+ % (dec fov))                        ;; (dec) because we want to land on the last fov'th line
        text        (-> ctx (r/input-area) (i/move-y bottom-line))]
    (-> ctx (r/with-text text) (r/refresh))))

(s/defn at-main-view-start :- Context
  [ctx :- Context]
  (-> ctx (at-input-end) (at-line-start) (at-view-top)))

(s/defn at-main-view-end :- Context
  [ctx :- Context]
  (-> ctx (at-input-end) (at-view-bottom)))

(s/defn shrink-view :- Context
   [ctx :- Context, n :- s/Int]
  (let [new-size (-> ctx (r/terminal) (t/size) (-- n))
        terminal (test-terminal {:size (constantly new-size)})]
    (-> ctx
        (r/with-terminal terminal)
        (process-one refresh))))

(s/defn enlarge-view :- Context
   [ctx :- Context, n :- s/Int]
  (let [new-size (-> ctx (r/terminal) (t/size) (+ n))
        terminal (test-terminal {:size (constantly new-size)})]
    (-> ctx
        (r/with-terminal terminal)
        (process-one refresh))))

(s/defn maximise-view :- Context
    [ctx :- Context]
  (let [height   (-> ctx (r/preview-hud) (h/text) (:height))
        terminal (test-terminal {:size (constantly height)})]
    (-> ctx
        (r/with-terminal terminal)
        (process-one refresh))))

(s/defn highlight-from :- r/Highlight
  [region :- Region]
  {:region region
   :scheme (c/normalise-palette c/default-syntax)
   :styles []})