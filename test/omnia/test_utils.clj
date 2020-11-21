(ns omnia.test-utils
  (:require omnia.hud
            omnia.input
            [clojure.test.check.generators :as gen]
            [clojure.test :refer [is]]
            [schema.core :as s]
            [omnia.more :refer [--]]
            [omnia.config :as c]
            [omnia.input :as i]
            [omnia.hud :as h]
            [omnia.repl :as r]
            [omnia.server :as server]
            [omnia.terminal :as t]
            [omnia.event :as e]))

(defn one [generator] (rand-nth (gen/sample generator)))

(defn many
  ([generator] (many generator (rand-int 100)))
  ([generator n] (vec (repeatedly n #(one generator)))))

(defmacro <=>seeker [this-seeker that-seeker]
  `(is (= (:lines ~this-seeker) (:lines ~that-seeker))
       (str "Failed for inputs: \n" ~this-seeker " :: \n" ~that-seeker)))

(defmacro <=>hud [this-hud that-hud]
  `(<=>seeker (:seeker ~this-hud) (:seeker ~that-hud)))

(s/defn has-seeker [hud :- h/Hud seeker :- i/Seeker]
  (is (= (:seeker hud) seeker)))

(defmacro can-be [val & fs]
  `(do ~@(map (fn [f#] `(is (~f# ~val) (str "Failed for input: \n" ~val))) fs)))

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

(defn gen-suggestions [size]
  (->> (gen/vector (gen/not-empty gen/string-alphanumeric) size)
       (gen/fmap
         (fn [xs]
           (list {:completions
                  (map (fn [s] {:candidate s
                                :ns        ""
                                :type      ""}) xs)})))))

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

(defn gen-context [{:keys [size fov seeker receive history]
                    :or   {size    0
                           fov     10
                           seeker  i/empty-seeker
                           history []}}]
  (->> (gen-seeker-of size)
       (gen/fmap
         (fn [hud-seeker]
           (let [hud (-> (h/hud fov) (h/enrich-with [hud-seeker]))]
             (-> (r/context (c/convert c/default-config)
                            (test-terminal {:size (constantly fov)})
                            (server/repl {:host    ""
                                          :port    0
                                          :history history
                                          :client  (constantly receive)}))
                 (r/seek seeker)
                 (r/persist hud)
                 (r/rebase)
                 (r/remember)))))))

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
(def scroll-up (e/event e/scroll-up))
(def scroll-down (e/event e/scroll-down))
(defn char-key [k] (e/event e/character k))
(def clear (e/event e/clear))
(def evaluate (e/event e/evaluate))
(def prev-eval (e/event e/prev-eval))
(def next-eval (e/event e/next-eval))
(def parens-match (e/event e/match))
(def suggest (e/event e/suggest))
(def ignore (e/event e/ignore))
(def backspace (e/event e/backspace))

(defn process
  ([ctx event]
   (process ctx event 1))
  ([ctx event n]
   (->> (range 0 n)
        (reduce (fn [nctx _] (-> nctx (r/process event) (:ctx))) ctx))))

(defn fov [ctx]
  (get-in ctx [:complete-hud :fov]))

(defn ov [ctx]
  (get-in ctx [:complete-hud :ov]))

(defn lor [ctx]
  (get-in ctx [:complete-hud :lor]))

(defn y [ctx]
  (get-in ctx [:complete-hud :seeker :cursor 1]))

(defn project-y [ctx]
  (let [complete (:complete-hud ctx)
        [_ y] (-> complete :seeker :cursor)]
    (h/project-y complete y)))

(defn project-complete [ctx]
  (h/project-hud (:complete-hud ctx)))

(defn project-cursor [ctx]
  (h/project-cursor (:complete-hud ctx)))

(defn project-highlight [ctx h-key]
  (let [complete  (:complete-hud ctx)
        selection (-> (:highlights ctx) (get h-key) (:region))]
    (h/project-selection complete selection)))

(defn no-projection [ctx]
  (let [complete (:complete-hud ctx)]
    {:start [0 (h/bottom-y complete)]
     :end   [0 (h/bottom-y complete)]}))

(defn shrink-by [ctx n]
  (update ctx :terminal (fn [term] (assoc term :size (constantly (-- (t/size term) n))))))

(defn enlarge-by [ctx n]
  (update ctx :terminal (fn [term] (assoc term :size (constantly (+ (t/size term) n))))))

(defn make-total [ctx]
  (let [h (get-in ctx [:complete-hud :seeker :height])]
    (-> ctx
        (assoc :terminal (test-terminal {:size (fn [] h)}))
        (assoc-in [:persisted-hud :fov] h)
        (assoc-in [:persisted-hud :lor] h)
        (r/rebase)
        (r/remember))))

(defn cursor [ctx]
  (get-in ctx [:complete-hud :seeker :cursor]))

(defn suggestions [ctx]
  (-> (:repl ctx) (server/complete! i/empty-seeker)))

(defn evaluation [seeker]
  {:value (i/stringify seeker)})

(defn history [ctx]
  (get-in ctx [:repl :history]))

(defn highlights? [highlited region]
  (let [{expected-start :start
         expected-end   :end} region
        {actual-start :start
         actual-end   :end} (:region highlited)]
    (and (= expected-start actual-start)
         (= expected-end actual-end))))

(defn empty-garbage [ctx]
  (assoc ctx :garbage i/empty-vec))

(defn move-start-fov [ctx]
  (->> (update ctx :seeker (comp i/start-x i/start-y))
       (r/rebase)
       (r/remember)))

(defn move-end-fov [ctx]
  (->> (update ctx :seeker (comp i/start-x i/end))
       (r/rebase)
       (r/remember)))

(defn move-top-fov [ctx]
  (let [fov (get-in ctx [:complete-hud :fov])
        top #(-- % (dec fov))]                              ;; (dec) because you want to land on the fov'th line
    (-> (move-end-fov ctx)
        (update :seeker #(i/move-y % top))
        (r/rebase)
        (r/remember))))

(defn move-bottom-fov [ctx]
  (let [fov    (get-in ctx [:complete-hud :fov])
        bottom #(+ % (dec fov))]
    (-> (update ctx :seeker #(i/move-y % bottom))
        (r/rebase)
        (r/remember))))

(defn from-start [ctx]
  (-> ctx
      (update-in [:persisted-hud :seeker] i/start-x)
      (update :seeker i/start-x)
      (r/rebase)
      (r/remember)))

(defn from-end [ctx]
  (-> ctx
      (update-in [:persisted-hud :seeker] i/end-x)
      (update :seeker i/end-x)
      (r/rebase)
      (r/remember)))