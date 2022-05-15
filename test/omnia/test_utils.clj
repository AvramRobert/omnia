(ns omnia.test-utils
  (:require [schema.core :as s]
            [clojure.core.match :as m]
            [omnia.config.core :as c]
            [omnia.repl.text :as i]
            [omnia.repl.hud :as h]
            [omnia.repl.context :as r]
            [omnia.repl.nrepl :as n]
            [omnia.view.terminal :as t]
            [omnia.util.collection :refer [reduce-idx]]
            [omnia.config.defaults :refer [default-user-config default-user-highlighting]]
            [omnia.schema.context :refer [Context]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.render :refer [HighlightInstructionData]]
            [omnia.schema.terminal :refer [TerminalSpec]]
            [omnia.schema.text :refer [Text Line]]
            [omnia.schema.hud :refer [View]]
            [omnia.schema.event :refer [Event]]
            [omnia.schema.common :refer [Region]]
            [omnia.schema.nrepl :refer [ValueResponse CompletionResponse NReplClient NReplResponse]])
  (:import (java.util UUID)))

(s/def default-config :- Config
  (c/convert default-user-config))

(s/def default-host :- s/Str
  "")

(s/def default-port :- s/Int
  0)

(s/def default-header :- Text
  (->> (r/header default-host default-port)
       (i/joined)))

(s/defn nrepl-client :- NReplClient
  ([nrepl-response :- NReplResponse]
   (nrepl-client nrepl-response []))
  ([nrepl-response :- NReplResponse
    history :- [Text]]
   (n/client {:host    default-host
              :port    default-port
              :history history
              :client  (constantly [nrepl-response])})))

(s/defn terminal :- t/Terminal
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

(s/defn process :- Context
  [ctx :- Context, events :- [Event]]
  (reduce (fn [ctx' event]
            (-> ctx'
                (r/process default-config event)
                (:context))) ctx events))

(s/defn highlight-from :- HighlightInstructionData
  [region :- Region]
  {:region region
   :scheme default-user-highlighting
   :styles []})

(s/defn value-response :- ValueResponse
  [value :- s/Str]
  {:id      (str (UUID/randomUUID))
   :session (str (UUID/randomUUID))
   :ns      "test-namespace"
   :status  ["done"]
   :value   value})

(s/defn completion-response :- CompletionResponse
  [completions :- [s/Str]]
  {:id          (str (UUID/randomUUID))
   :session     (str (UUID/randomUUID))
   :ns          "test-ns"
   :status      ["done"]
   :completions (mapv (fn [c]
                        {:candidate c
                         :type      "var"
                         :ns        "candidate-ns"}) completions)})

(s/defn derive-text :- Text
  [strings :- [s/Str]]
  "Reads inputs strings into a text.
   The strings are allowed markers specifying a cursor position and a possible selection range.
   Cursor identified through: |
   Selection identified through: ⦇ (start: U+2987), ⦈ (end: U+2988)
   Example:
    ['hel|l⦇o' 'worl⦈d'] => [0, 3] ; [[o] [w o r l]]"
  (letfn [(point [{:keys [y find remove latest]} line]
            (->> line
                 (filter #(not (contains? remove %)))
                 (reduce-idx (fn [x p c] (if (= c find) [x, y] p)) latest)))]
    (loop [[s & ss] strings
           lines  []
           cursor [0 0]
           start  nil
           end    nil]
      (if (some? s)
        (recur
          ss
          (->> s (filter #(not (contains? #{\| \⦇ \⦈} %))) (vec) (conj lines))
          (point {:remove #{\⦇ \⦈}
                  :find   \|
                  :latest cursor
                  :y      (count lines)} s)
          (point {:remove #{\| \⦈}
                  :find   \⦇
                  :y      (count lines)
                  :latest start} s)
          (point {:remove #{\| \⦇}
                  :find   \⦈
                  :latest end
                  :y      (count lines)} s))
        (cond-> (i/create-text lines cursor)
                (and start end (not= start end)) (assoc :selection {:start start :end end}))))))

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
             [:input  :---] (recur input view offset [] cs fov voff soff parsing)
             [:input  :-|]  (recur persisted view offset input cs (inc fov) voff soff :view)
             [:view   :-|]  (recur persisted view offset input cs (inc fov) voff soff :view)
             [:view   :-+]  (recur persisted view offset input cs fov (inc voff) soff :offset)
             [:input  :-+]  (recur persisted view offset input cs fov (inc voff) soff :offset)
             [:offset :-+]  (recur persisted view offset input cs fov (inc voff) soff :offset)
             [_       :-$]  (recur persisted view offset input cs fov voff (inc soff) parsing)
             [:view   _]    (recur persisted (conj view c) offset (conj input c) cs fov voff soff :input)
             [:offset _]    (recur persisted view (conj offset c) (conj input c) cs fov voff soff :input)
             :else          (recur persisted view offset (conj input c) cs fov voff soff :input))))

(s/defn derive-context :- Context
  "Derives a context from a declarative list of tagged strings.
   Interprets the strings to Text as per `derive-text`.
   Highlights are however fed to the context highlights.

   Uses additional tags to derive a context for those strings.
   Tags:
     --- : end of a persisted area and start of an input area
              * if completely missing, the input area assumes the entire text lines in the def
     -|  : a line part of the field-of-view
              * if present, the total of these lines represent the field-of-view
              * if completely missing, the field-of-view is set to be the total amount of text lines in the def
     -+  : a line offset by the view
              * if present, the total of these lines represent the view-offset
     -$  : a line offset by the scroll
              * if present, the total of these lines represent the scroll-offset
   Example:
      [persisted
       ---
       line-0-not-viewable
       -| line-1-viewable
       -| line-2-viewable
       -| (invisible line, increments the field-of-view)
       -$ line-3-scrolled
       -$ (invisible line, increments the scroll-offset)
       -+ line-5-offset
       -+ (invisible line, increments the view-offset)]"
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
         parsed-input         (derive-text input-area)
         parsed-persisted     (if (empty? persisted-area)
                                []
                                [(derive-text persisted-area)])
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
                                  (derive-text)
                                  (:selection))]
     (as-> context ctx
           (r/with-persisted ctx persisted-hud)
           (r/with-input-area ctx parsed-input)
           (r/refresh ctx)
           (r/with-preview ctx (-> ctx
                                   (r/preview-hud)
                                   (h/with-view-offset view-offset)))
           (r/with-previous ctx (h/view-of field-of-view))
           (if (some? highlights)
             (r/with-highlight ctx :manual (r/make-manual default-config highlights))
             ctx)))))

(s/defn derive-hud :- View
  "Derives a hud based on a list of tagged strings.
   Behaves similarly to `derive-context`, but derives a simple hud.
   Ignores context-specific information like the persisted area."
  [def :- HudDefinition]
  (let [{:keys [input-area
                field-of-view
                view-offset
                scroll-offset]} (parse def)]
    (-> input-area
        (derive-text)
        (h/view field-of-view)
        (h/with-view-offset view-offset)
        (h/with-scroll-offset scroll-offset))))
