(ns omnia.test-utils
  (:require [clojure.test.check.generators :as gen]
            [clojure.test :refer [is]]
            [omnia.more :refer [--]]
            [omnia.config :refer [default-keymap default-cs]]
            [omnia.input :as i]
            [omnia.hud :as h]
            [omnia.repl :as r]
            [omnia.terminal :as t]
            [omnia.render :as rd]
            [clojure.string :as s]))

(defn one [generator] (rand-nth (gen/sample generator)))

(defn many
  ([generator] (many generator (rand-int 100)))
  ([generator n] (vec (repeatedly n #(one generator)))))

(defmacro <=> [this-seeker that-seeker]
  `(is (= (:lines ~this-seeker) (:lines ~that-seeker))
       (str "Failed for inputs: \n" ~this-seeker " :: \n" ~that-seeker)))

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
  (->> (gen/vector gen/string-alphanumeric size)
       (gen/fmap
         (fn [xs]
           (list {:completions
                  (map (fn [s] {:candidate s
                                :ns ""
                                :type ""}) xs)})))))

(defn test-terminal [{:keys [background!
                             foreground!
                             style!
                             un-style!
                             visible!
                             clear!
                             size
                             move!
                             put!
                             stop!
                             start!
                             keystroke!]
                      :as fns}]
  (assert (map? fns) "The input to `test-terminal` should be a map (look at omnia.test-utils)")
  (let [unit (constantly nil)]
    {:background! (or background! unit)
     :foreground! (or foreground! unit)
     :style!      (or style! unit)
     :un-style!   (or un-style! unit)
     :visible!    (or visible! unit)
     :clear!      (or clear! unit)
     :size        (or size (constantly 10))
     :move!       (or move! unit)
     :put!        (or put! unit)
     :stop!       (or stop! unit)
     :start!      (or start! unit)
     :keystroke!  (or keystroke! unit)}))

(defn gen-context [{:keys [size fov seeker receive history]
                    :or {size 0
                         fov 10
                         seeker i/empty-seeker
                         history []}}]
  (->> (gen-seeker-of size)
       (gen/fmap
         (fn [hud-seeker]
           (let [hud (h/hud fov hud-seeker)]
             (-> (h/context {:terminal (test-terminal {:size (constantly fov)})
                             :repl (-> (r/repl {:host    ""
                                                :port    0
                                                :history history
                                                :send!   (constantly receive)}))
                             :keymap default-keymap
                             :colourscheme default-cs})
                 (h/seek seeker)
                 (h/persist hud)
                 (h/rebase)
                 (h/remember)))))))

(defn event [action key]
  (i/->Event action key))

(def up (event :up :up))
(def down (event :down :down))
(def left (event :left :left))
(def right (event :right :right))
(def select-all (event :select-all \a))
(def select-down (event :select-down :down))
(def select-up (event :select-up :up))
(def select-right (event :select-right :right))
(def select-left (event :select-left :left))
(def expand (event :expand \w))
(def copy (event :copy \c))
(def paste (event :paste \v))
(def backspace (event :backspace :backspace))
(def enter (event :newline :enter))
(def scroll-up (event :scroll-up :page-up))
(def scroll-down (event :scroll-down :page-down))
(defn char-key [k] (event :char k))
(def clear (event :clear \r))
(def evaluate (event :eval \e))
(def prev-eval (event :prev-eval :up))
(def next-eval (event :next-eval :down))
(def parens-match (event :match \p))
(def suggest (event :suggest :tab))

(defn process
  ([ctx event]
   (process ctx event 1))
  ([ctx event n]
   (->> (range 0 n)
        (reduce (fn [nctx _] (second (h/process nctx event))) ctx))))

(defn fov [ctx]
  (get-in ctx [:complete-hud :fov]))

(defn ov [ctx]
  (get-in ctx [:complete-hud :ov]))

(defn lor [ctx]
  (get-in ctx [:complete-hud :lor]))

(defn y [ctx]
  (get-in ctx [:complete-hud :cursor 1]))

(defn project-y [ctx]
  (let [complete (:complete-hud ctx)
        [_ y] (:cursor complete)]
    (rd/project-y complete y)))

(defn project-complete [ctx]
  (rd/project-hud (:complete-hud ctx)))

(defn project-cursor [ctx]
  (rd/project-cursor (:complete-hud ctx)))

(defn project-highlight [ctx h-key]
  (let [complete (:complete-hud ctx)
        selection (-> (:highlights ctx) (get h-key) (:region))]
    (rd/project-selection complete selection)))

(defn no-projection [ctx]
  (let [complete (:complete-hud ctx)]
    {:start [0 (rd/bottom-y complete)]
     :end   [0 (rd/bottom-y complete)]}))

(defn shrink-by [ctx n]
  (update ctx :terminal (fn [term] (assoc term :size (constantly (-- (t/size term) n))))))

(defn enlarge-by [ctx n]
  (update ctx :terminal (fn [term] (assoc term :size (constantly (+ (t/size term) n))))))

(defn make-total [ctx]
  (let [h (get-in ctx [:complete-hud :height])]
    (-> ctx
        (assoc :terminal (test-terminal {:size (fn [] h)}))
        (assoc-in [:persisted-hud :fov] h)
        (assoc-in [:persisted-hud :lor] h)
        (h/rebase)
        (h/remember))))

(defn cursor [ctx]
  (get-in ctx [:complete-hud :cursor]))

(defn suggestions [ctx]
  (-> (:repl ctx) (r/complete! i/empty-seeker)))

(defn signatures [ctx]
  (letfn [(sign [{:keys [ns name args]}]
            (->> args
                 (mapv #(str ns "/" name " " %))
                 (s/join "\n")
                 (i/from-string)))]
    (-> (:repl ctx)
        (r/info! i/empty-seeker)
        (sign))))

(defn evaluation [seeker]
  {:value (i/stringify seeker)})

(defn history [ctx]
  (get-in ctx [:repl :history]))

(defn highlights? [highlited region]
  (let [{expected-start :start
         expected-end   :end} region
        {actual-start   :start
         actual-end     :end} (:region highlited)]
    (and (= expected-start actual-start)
         (= expected-end actual-end))))

(defn empty-garbage [ctx]
  (assoc ctx :garbage i/empty-vec))

(defn move-start-fov [ctx]
  (->> (update ctx :seeker (comp i/start-x i/start-y))
       (h/rebase)
       (h/remember)))

(defn move-end-fov [ctx]
  (->> (update ctx :seeker (comp i/start-x i/end))
       (h/rebase)
       (h/remember)))

(defn move-top-fov [ctx]
  (let [fov (get-in ctx [:complete-hud :fov])
        top #(-- % (dec fov))]                              ;; (dec) because you want to land on the fov'th line
    (-> (move-end-fov ctx)
        (update :seeker #(i/move-y % top))
        (h/rebase)
        (h/remember))))

(defn move-bottom-fov [ctx]
  (let [fov (get-in ctx [:complete-hud :fov])
        bottom #(+ % (dec fov))]
    (-> (update ctx :seeker #(i/move-y % bottom))
        (h/rebase)
        (h/remember))))

(defn from-start [ctx]
  (-> ctx
      (update :persisted-hud i/start-x)
      (update :seeker i/start-x)
      (h/rebase)
      (h/remember)))

(defn from-end [ctx]
  (-> ctx
      (update :persisted-hud i/end-x)
      (update :seeker i/end-x)
      (h/rebase)
      (h/remember)))