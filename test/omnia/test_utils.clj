(ns omnia.test-utils
  (:require [schema.core :as s]
            [clojure.test.check.generators :as gen]
            [clojure.core.match :as m]
            [omnia.config.core :as c]
            [omnia.repl.text :as i]
            [omnia.repl.hud :as h]
            [omnia.repl.context :as r]
            [omnia.repl.nrepl :as server]
            [omnia.view.terminal :as t]
            [omnia.repl.events :as e]
            [clojure.test :refer [is]]
            [omnia.schema.context :refer [Context]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.render :refer [HighlightInfo HighlightType]]
            [omnia.schema.terminal :refer [TerminalSpec]]
            [omnia.schema.text :refer [Seeker Line]]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.event :refer [Event]]
            [omnia.schema.common :refer [Point Region]]
            [omnia.schema.nrepl :refer [NReplClient NReplResponse]]
            [omnia.util.arithmetic :refer [-- ++]]
            [omnia.util.generator :refer [do-gen one]]
            [omnia.config.defaults :refer [default-user-config default-user-highlighting]]
            [omnia.schema.nrepl :as n])
  (:import (java.util UUID)))

(s/def default-config :- Config
  (c/convert default-user-config))

(s/def default-host :- s/Str
  "")

(s/def default-port :- s/Int
  0)

(s/def default-header :- Seeker
  (->> (r/header default-host default-port)
       (i/joined)))

(s/defn nrepl-client :- NReplClient
  ([nrepl-response :- NReplResponse]
   (nrepl-client nrepl-response []))
  ([nrepl-response :- NReplResponse
    history :- [Seeker]]
   (server/client {:host    default-host
                   :port    default-port
                   :history history
                   :client  (constantly [nrepl-response])})))

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

(defn gen-text-area-of [size]
  (do-gen [text   (gen-text-of size)
           cursor (gen-cursor text)]
    (-> text (i/seeker) (i/reset-to cursor))))

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

(s/defn test-terminal :- t/Terminal
  [fns :- TerminalSpec]
  (let [unit (constantly nil)]
    (reify t/Terminal
      (clear! [t] ((:clear! fns unit) t))
      (refresh! [t] ((:refresh! fns unit) t))
      (size [t] ((:size fns (constantly 10)) t))
      (move! [t x y] ((:move! fns unit) t x y))
      (stop! [t] ((:stop! fns unit) t))
      (start! [t] ((:start! fns unit) t))
      (put! [t ch x y fg bg stls] ((:put! fns unit) t ch x y fg bg stls))
      (get-event! [t] ((:get-event! fns unit) t)))))

(defn gen-context [{:keys [prefilled-size
                           view-size
                           text-area
                           receive
                           history]
                    :or   {prefilled-size 0
                           view-size      10
                           receive        (gen/return {})
                           text-area      (gen/return i/empty-seeker)
                           history        (gen/return [])}}]
  (do-gen [hud-seeker      (gen-text-area-of prefilled-size)
           input-seeker    text-area
           response        (gen-nrepl-result receive)
           history-seekers history]
    (-> (r/context view-size
                   (server/client {:host    ""
                                   :port    0
                                   :history history-seekers
                                   :client  (constantly response)}))
        (r/with-input-area input-seeker)
        (r/with-hud (h/hud hud-seeker view-size)))))

(s/defn context-from :- Context
  [text :- Seeker]
  (-> {:text-area (gen/return text)} (gen-context) (one)))

(def up e/move-up)
(def down e/move-down)
(def left e/move-left)
(def right e/move-right)
(def select-all e/select-all)
(def select-down e/select-down)
(def select-up e/select-up)
(def select-right e/select-right)
(def select-left e/select-left)
(def jump-select-left e/jump-select-left)
(def jump-select-right e/jump-select-right)
(def expand e/expand)
(def copy e/copy)
(def paste e/paste)
(def enter e/new-line)
(def scroll-up e/scroll-up)
(def scroll-down e/scroll-down)
(defn character [k] (e/character k))
(def delete-previous e/delete-previous)

(s/defn process :- Context
  [ctx :- Context, events :- [Event]]
  (reduce (fn [ctx' event]
            (-> ctx'
                (r/process default-config event)
                (:context))) ctx events))

(s/defn overview :- s/Int
  [ctx :- Context]
  (-> ctx (r/preview-hud) (h/view-offset)))

(s/defn project-y :- s/Int
  [ctx :- Context]
  (let [view (r/preview-hud ctx)
        [_ y] (-> view (h/text) (:cursor))]
    (h/project-y view y)))

(s/defn project-preview :- Seeker
  [ctx :- Context]
  (h/project-hud (r/preview-hud ctx)))

(defn project-preview-cursor [ctx]
  (-> ctx (r/preview-hud) (h/project-hud-cursor)))

(defn cursor [ctx]
  (-> ctx (r/preview-hud) (h/text) (:cursor)))

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
  (let [fov      (r/view-size ctx)
        top-line #(-- % (dec fov))                          ;; (dec) because we want to land on the fov'th line
        text     (-> ctx (r/input-area) (i/move-y top-line))]
    (-> ctx (r/with-input-area text) (r/refresh))))

(s/defn at-view-bottom :- Context
  [ctx :- Context]
  (let [fov         (r/view-size ctx)
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
  (let [new-size (-> ctx (r/view-size) (++ n))]
    (process ctx [(e/resize 80 new-size)])))

(s/defn maximise-view :- Context
  [ctx :- Context]
  (let [view-size (r/view-size ctx)
        text-size (-> ctx (r/preview-hud) (h/text) (:size))]
    (resize-view-by ctx (- text-size view-size))))

(s/defn extend-highlight :- Context
  [ctx :- Context,
   h-type :- HighlightType,
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

(s/defn highlight-from :- HighlightInfo
  [region :- Region]
  {:region region
   :scheme default-user-highlighting
   :styles []})

(s/defn value-response :- n/ValueResponse
  [value :- s/Str]
  {:id      (str (UUID/randomUUID))
   :session (str (UUID/randomUUID))
   :ns      "test-namespace"
   :status  ["done"]
   :value   value})

(s/defn completion-response :- n/CompletionResponse
  [completions :- [s/Str]]
  {:id          (str (UUID/randomUUID))
   :session     (str (UUID/randomUUID))
   :ns          "test-ns"
   :status      ["done"]
   :completions (mapv (fn [c]
                        {:candidate c
                         :type      "var"
                         :ns        "candidate-ns"}) completions)})

(s/def --- :- s/Keyword
  :---)

(s/def -+ :- s/Keyword
  :-+)

(s/def -$ :- s/Keyword
  :-$)

(s/def -| :- s/Keyword
  :-|)

(s/def Input-Area-Definition (s/eq ---))
(s/def View-Area-Definition (s/eq -|))
(s/def Offset-Area-Definition (s/eq -+))
(s/def Scroll-Area-Definition (s/eq -$))
(s/def TextDefinition s/Str)

(s/def HudDefinition (s/cond-pre View-Area-Definition TextDefinition Offset-Area-Definition Scroll-Area-Definition))
(s/def ContextDefinition (s/cond-pre HudDefinition Input-Area-Definition))

(s/def NReplProps {(s/maybe :response) NReplResponse
                   (s/maybe :history)  [s/Str]})

(s/def ContextProps
  {:input-area     [Line]
   :persisted-area [Line]
   :view-area      [Line]
   :hidden-area    [Line]
   :field-of-view  s/Int
   :view-offset    s/Int
   :scroll-offset  s/Int})

(s/defn parse :- ContextProps
  "Parses a vector of potentially tagged lines to context-relevant information.
   Tags:
     --- : end of a persisted area and start of an input area
          :* if completely missing, the input area assumes the entire text lines in the def
     -|  : a line part of the field-of-view
          :* if present, the total of these lines represent the field-of-view
          :* if completely missing, the field-of-view is set to be the total amount of text lines in the def
     -+  : a line offset by the view
          :* if present, the total of these lines represent the view-offset
     -$  : a line offset by the scroll
          :* if present, the total of these lines represent the scroll-offset

   Example:
      [persisted
       area
       ---
       line-0-not-viewable
       -| line-1-viewable
       -| line-2-viewable
       -$ line-3-scrolled
       -$ line-4-scrolled
       -+ line-5-offset]"
  [def :- ContextDefinition]
  (loop [persisted []
         view      []
         offset    []
         input     []
         [c & cs]  def
         fov       0
         voff      0
         soff      0
         parsing   :input]
    (m/match [parsing c]
             [_ nil] {:persisted-area persisted
                      :view-area      view
                      :hidden-area    offset
                      :input-area     input
                      :field-of-view  (if (zero? fov) (count (concat input persisted)) fov)
                      :view-offset    voff
                      :scroll-offset  soff}
             [:input :---] (recur input view offset [] cs fov voff soff parsing)
             [:input :-|]  (recur persisted view offset input cs (inc fov) voff soff :view)
             [:view  :-|]  (recur persisted view offset input cs (inc fov) voff soff :view)
             [:view  :-+]  (recur persisted view offset input cs fov (inc voff) soff :offset)
             [:input :-+]  (recur persisted view offset input cs fov (inc voff) soff :offset)
             [_      :-$]  (recur persisted view offset input cs fov voff (inc soff) parsing)
             [:view   _]   (recur persisted (conj view c) offset (conj input c) cs fov voff soff :input)
             [:offset _]   (recur persisted view (conj offset c) (conj input c) cs fov voff soff :input)
             :else         (recur persisted view offset (conj input c) cs fov voff soff :input))))

(s/defn derive-context :- Context
  ([def :- ContextDefinition]
   (derive-context def {}))
  ([def :- ContextDefinition
    props :- NReplProps]
   (let [{:keys [input-area
                 persisted-area
                 field-of-view
                 view-offset
                 scroll-offset]} (parse def)
         nrepl-client         (nrepl-client (:response props {})
                                            (->> []
                                                 (:history props)
                                                 (reverse)
                                                 (mapv i/from-string)))
         context              (r/context field-of-view nrepl-client)
         parsed-input         (i/from-tagged-strings input-area)
         parsed-persisted     (if (empty? persisted-area)
                                []
                                [(i/from-tagged-strings persisted-area)])
         persisted-hud        (-> context
                                  (r/persisted-hud)
                                  (h/enrich-with parsed-persisted)
                                  (h/with-view-offset view-offset)
                                  (h/with-scroll-offset scroll-offset))
         highlights           (-> context
                                  (r/persisted-hud)
                                  (h/text)
                                  (:lines)
                                  (concat persisted-area input-area)
                                  (i/from-tagged-strings)
                                  (:selection))]
     (as-> context ctx
           (r/with-persisted ctx persisted-hud)
           (r/with-input-area ctx parsed-input)
           (r/refresh ctx)
           (r/with-preview ctx (-> ctx
                                   (r/preview-hud)
                                   (h/with-view-offset view-offset)))
           (r/with-previous ctx (h/hud-of field-of-view))
           (if (some? highlights)
             (r/with-highlight ctx :manual (r/make-manual default-config highlights))
             ctx)))))

(s/defn derive-hud :- Hud
  [def :- HudDefinition]
  (let [{:keys [input-area
                field-of-view
                view-offset
                scroll-offset]} (parse def)]
    (-> input-area
        (i/from-tagged-strings)
        (h/hud field-of-view)
        (h/with-view-offset view-offset)
        (h/with-scroll-offset scroll-offset))))