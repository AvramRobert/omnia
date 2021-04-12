(ns omnia.test-utils
  (:require [clojure.test :refer [is]]
            [omnia.repl.context :refer [Context HighlightType]]
            [omnia.text.core :refer [Seeker]]
            [omnia.config.components.event :refer [Event TextEvent]]
            [omnia.util.schema :refer [Point Region]]
            [omnia.util.arithmetic :refer [-- ++]]
            [omnia.util.generator :refer [do-gen]]
            [omnia.repl.hud :refer [Hud]]
            [schema.core :as s]
            [clojure.test.check.generators :as gen]
            [omnia.config.defaults :refer [default-user-highlighting]]
            [omnia.config.core :as c]
            [omnia.text.core :as i]
            [omnia.repl.hud :as h]
            [omnia.repl.context :as r]
            [omnia.repl.nrepl :as server]
            [omnia.view.terminal :as t]
            [omnia.config.components.event :as e]))

(defmacro should-be [val & fs]
  `(do ~@(map (fn [f#] `(is (~f# ~val) (str "Failed for input: \n" ~val))) fs)))

(defn gen-coordinate [space]
  (if (empty? space)
    (gen/return 0)
    (gen/elements (range 0 (count space)))))

(defn gen-cursor [text]
  (do-gen [y (gen-coordinate text)
           x (gen-coordinate (get text y))]
    [x y]))

(defn gen-line-of [size]
  (gen/vector gen/char-alphanumeric size))

(def gen-nonempty-line
  (do-gen [size (gen/choose 1 10)
           line (gen-line-of size)]
    line))

(defn gen-text-of [size]
  (gen/vector gen-nonempty-line size))

(def gen-nonempty-text
  (do-gen [size (gen/choose 1 10)
           text (gen-text-of size)]
    text))

(def gen-seeker
  (do-gen [text   gen-nonempty-text
           cursor (gen-cursor text)]
    (-> text (i/seeker) (i/reset-to cursor))))

(defn gen-seeker-of [size]
  (do-gen [text   (gen-text-of size)
           cursor (gen-cursor text)]
    (-> text (i/seeker) (i/reset-to cursor))))

(defn gen-completion [size]
  (do-gen [candidates (-> gen/string-alphanumeric (gen/not-empty) (gen/vector size))]
    {:completions (mapv (fn [s]
                          {:candidate s
                           :ns        ""
                           :type      ""}) candidates)}))

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

(defn gen-history [{:keys [size element-size]}]
  (gen/vector (gen-seeker-of element-size) size))

(s/defn test-terminal :- t/Terminal
  [fns :- t/TerminalSpec]
  (let [unit (constantly nil)]
    (reify t/Terminal
      (clear! [t]                 ((:clear! fns unit) t))
      (refresh! [t]               ((:refresh! fns unit) t))
      (size [t]                   ((:size fns (constantly 10)) t))
      (move! [t x y]              ((:move! fns unit) t x y))
      (stop! [t]                  ((:stop! fns unit) t))
      (start! [t]                 ((:start! fns unit) t))
      (put! [t ch x y fg bg stls] ((:put! fns unit) t ch x y fg bg stls))
      (get-event! [t]             ((:get-event! fns unit) t)))))

(defn gen-context [{:keys [size fov seeker receive history]
                    :or   {size    0
                           fov     10
                           receive (gen/return {})
                           seeker  (gen/return i/empty-seeker)
                           history (gen/return [])}}]
  (do-gen [hud-seeker      (gen-seeker-of size)
           input-seeker    seeker
           response        (gen-nrepl-result receive)
           history-seekers history]
    (-> (r/context (c/convert c/default-user-config)
                   (test-terminal {:size (constantly fov)})
                   (server/client {:host    ""
                                   :port    0
                                   :history history-seekers
                                   :client  (constantly response)}))
        (r/with-input-area input-seeker)
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

(s/defn process :- Context
  [ctx :- Context, events :- [Event]]
  (reduce (comp :ctx r/process) ctx events))

(s/defn process' :- Seeker
  [seeker :- Seeker, events :- [TextEvent]]
  (reduce i/process seeker events))

(s/defn overview :- s/Int
  [ctx :- Context]
  (-> ctx (r/preview-hud) (h/view-offset)))

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

(defn project-preview-cursor [ctx]
  (-> ctx (r/preview-hud) (h/project-hud-cursor)))

(defn cursor [ctx]
  (-> ctx (r/preview-hud) (h/text) (:cursor)))

(defn suggestions [ctx]
  (-> ctx (r/client) (server/complete! i/empty-seeker) (server/result)))

(s/defn suggestion-at :- i/Line
  [ctx :- Context
   line :- s/Int]
  (-> ctx (suggestions) (i/reset-y line) (i/line)))

(defn server-history [ctx]
  (-> ctx (r/client) (:history)))

(defn highlights? [highlighted region]
  (let [{expected-start :start
         expected-end   :end} region
        {actual-start :start
         actual-end   :end} (:region highlighted)]
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
    (-> ctx (r/with-input-area text) (r/refresh))))

(s/defn at-input-end :- Context
  [ctx :- Context]
  (let [text (-> ctx (r/input-area) (i/end))]
    (-> ctx (r/with-input-area text) (r/refresh))))

(s/defn at-line-start :- Context
  [ctx :- Context]
  (let [text (-> ctx (r/input-area) (i/start-x))]
    (-> ctx (r/with-input-area text) (r/refresh))))

(s/defn at-line-end :- Context
  [ctx :- Context]
  (let [text (-> ctx (r/input-area) (i/end-x))]
    (-> ctx (r/with-input-area text) (r/refresh))))

(s/defn at-view-top :- Context
  [ctx :- Context]
  (let [fov       (-> ctx (r/preview-hud) (h/field-of-view))
        top-line #(-- % (dec fov))                          ;; (dec) because we want to land on the fov'th line
        text      (-> ctx (r/input-area) (i/move-y top-line))]
    (-> ctx (r/with-input-area text) (r/refresh))))

(s/defn at-view-bottom :- Context
  [ctx :- Context]
  (let [fov         (-> ctx (r/preview-hud) (h/field-of-view))
        bottom-line #(+ % (dec fov))                        ;; (dec) because we want to land on the last fov'th line
        text        (-> ctx (r/input-area) (i/move-y bottom-line))]
    (-> ctx (r/with-input-area text) (r/refresh))))

(s/defn at-main-view-start :- Context
  [ctx :- Context]
  (-> ctx (at-input-end) (at-line-start) (at-view-top)))

(s/defn at-main-view-end :- Context
  [ctx :- Context]
  (-> ctx (at-input-end) (at-view-bottom)))

(s/defn resize-view-by :- Context
  [ctx :- Context, n :- s/Int]
  (let [new-size (-> ctx (r/preview-hud) (h/field-of-view) (++ n))]
    (process ctx [(e/resize-event 80 new-size)])))

(s/defn maximise-view :- Context
    [ctx :- Context]
  (let [size   (-> ctx (r/preview-hud) (h/field-of-view))
        height (-> ctx (r/preview-hud) (h/text) (:height))]
    (resize-view-by ctx (- height size))))

(s/defn extend-highlight :- Context
  [ctx         :- Context,
   h-type      :- HighlightType,
   [xoff yoff] :- Point]
  (let [highlight (-> ctx
                      (r/highlights)
                      (get h-type)
                      (update
                        :region
                        (fn [{[xs ys] :start
                              [xe ye] :end}]
                          {:start [(+ xs xoff) (+ ys yoff)]
                           :end   [(+ xe xoff) (+ ye yoff)]})))]
    (r/with-highlight ctx h-type highlight)))

(s/defn highlight-from :- r/Highlight
  [region :- Region]
  {:region region
   :scheme default-user-highlighting
   :styles []})