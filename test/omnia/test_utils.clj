(ns omnia.test-utils
  (require [clojure.test.check.generators :as gen]
           [clojure.test :refer [is]]
           [omnia.more :refer [--]]
           [omnia.config :refer [default-keymap default-cs]]
           [omnia.input :as i]
           [omnia.hud :as h]
           [omnia.repl :as r]
           [omnia.terminal :as t]
           [omnia.rendering :as rd]))

(defn one [generator] (rand-nth (gen/sample generator)))

(defn many
  ([generator] (many (rand-int 100)))
  ([generator n] (vec (repeatedly n #(one generator)))))

(defmacro <=> [this-seeker that-seeker]
  `(is (= (:lines ~this-seeker) (:lines ~that-seeker))
       (str "Failed for inputs: \n" ~this-seeker " :: \n" ~that-seeker)))

(defmacro can-be [val & fs]
  `(do ~@(map (fn [f#] `(is (~f# ~val) (str "Failed for input: \n" ~val))) fs)))

(defn rand-cursor [seeker]
  (let [y (-> seeker (i/height) (rand-int))
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

(defn test-terminal [size]
  (t/map->Terminal
    {:background! (constantly nil)
     :foreground! (constantly nil)
     :clear!      (constantly nil)
     :size        (fn [] size)
     :move!       (constantly nil)
     :put!        (constantly nil)
     :stop!       (constantly nil)
     :start!      (constantly nil)
     :keystroke!  (constantly nil)}))

(def ctx (h/context {:terminal nil
                     :repl nil
                     :keymap default-keymap
                     :colourscheme default-cs}))

(defn gen-context [{:keys [size fov seeker suggestions history]
                    :or {fov 10
                         suggestions i/empty-seeker
                         history []
                         seeker (:seeker ctx)}}]
  (assert (not (nil? size)) "A context must always have a hud size")
  (gen/fmap
    (fn [hud-seeker]
      (let [hud (h/hud fov hud-seeker)]
        (-> ctx
            (assoc :terminal (test-terminal fov)
                   :repl (-> (r/repl {:kind :identity
                                      :history history})
                             (assoc :complete-f (constantly suggestions)))
                   :seeker seeker
                   :complete-hud hud
                   :persisted-hud hud
                   :previous-hud hud)
            (h/rebase seeker)
            (h/remember)))) (gen-seeker-of size)))

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
(def copy (event :copy \c))
(def paste (event :paste \v))
(def backspace (event :backspace :backspace))
(def enter (event :enter :enter))
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

(defn project-hud [ctx]
  (rd/project-hud (:complete-hud ctx)))

(defn project-cursor [ctx]
  (rd/project-cursor (:complete-hud ctx)))

(defn project-selection [ctx]
  (let [fov (get-in ctx [:complete-hud :fov])
        selection (first (:highlights ctx))]
    (rd/project-selection selection fov)))

(defn make-total [ctx]
  (let [h (get-in ctx [:complete-hud :height])]
    (-> ctx
        (assoc :terminal (test-terminal h))
        (assoc-in [:persisted-hud :fov] h)
        (assoc-in [:persisted-hud :lor] h)
        (h/rebase)
        (h/remember))))

(defn cursor [ctx]
  (get-in ctx [:complete-hud :cursor]))

(defn suggestions [ctx]
  ((get-in ctx [:repl :complete-f])))

(defn history [ctx]
  (get-in ctx [:repl :history]))

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