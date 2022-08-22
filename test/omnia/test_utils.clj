(ns omnia.test-utils
  (:require [schema.core :as s]
            [clojure.core.match :as m]
            [omnia.config.core :as config]
            [omnia.repl.text :as i]
            [omnia.repl.view :as v]
            [omnia.repl.hud :as h]
            [omnia.repl.context :as c]
            [omnia.repl.nrepl :as n]
            [omnia.repl.eval-history :as eh]
            [omnia.display.terminal :as t]
            [clojure.string :as string]
            [omnia.util.collection :refer [reduce-idx map-vals]]
            [omnia.config.defaults :refer [default-user-config]]
            [omnia.schema.context :refer [Context]]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.eval-history :refer [EvalHistory]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.render :refer [HighlightInstructionData]]
            [omnia.schema.terminal :refer [Terminal]]
            [omnia.schema.text :refer [Text Line]]
            [omnia.schema.view :refer [View]]
            [omnia.schema.event :refer [Event]]
            [omnia.schema.common :refer [Region]]
            [omnia.schema.nrepl :refer [ValueResponse
                                        CompletionResponse
                                        DocResponse
                                        ArgumentResponse
                                        TerminatingResponse
                                        NReplClient
                                        NReplResponse]])
  (:import (java.util UUID)))

(def TerminalSpec
  {(s/optional-key :move!)      s/Any
   (s/optional-key :put!)       s/Any
   (s/optional-key :size)       s/Any
   (s/optional-key :clear!)     s/Any
   (s/optional-key :refresh!)   s/Any
   (s/optional-key :stop!)      s/Any
   (s/optional-key :start!)     s/Any
   (s/optional-key :get-event!) s/Any})

(s/def default-config :- Config
  (config/convert "." default-user-config))

(s/def default-header :- Text h/header)

(s/defn nrepl-client :- NReplClient
  [nrepl-response :- NReplResponse]
  (n/client {:host   "nrepl://test-host"
             :port   0
             :client (constantly [nrepl-response])}))

(s/defn terminal :- Terminal
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

(s/defn highlight-from :- HighlightInstructionData
  [region :- Region]
  {:region region
   :scheme (-> default-config (:syntax) (:standard))
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

(s/defn doc-response :- DocResponse
  [doc :- s/Str]
  {:id      (str (UUID/randomUUID))
   :session (str (UUID/randomUUID))
   :ns      "test-ns"
   :status  ["done"]
   :name    "test-name"
   :doc     doc})

(s/defn argument-response :- ArgumentResponse
  [namespace :- s/Str
   name :- s/Str
   args :- [s/Str]]
  {:id           (str (UUID/randomUUID))
   :session      (str (UUID/randomUUID))
   :ns           namespace
   :status       ["done"]
   :name         name
   :arglists-str (string/join "\n" args)})

(s/def terminating-response :- TerminatingResponse
  {:id      (str (UUID/randomUUID))
   :session (str (UUID/randomUUID))
   :status  ["done"]})

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
                (and start end (not= start end)) (assoc :selection {:from start :until end}))))))

(s/def --- :- s/Keyword
  :---)

(s/def -+ :- s/Keyword
  :-+)

(s/def -$ :- s/Keyword
  :-$)

(s/def -| :- s/Keyword
  :-|)

(s/def Input-Area-Tag (s/eq ---))
(s/def Viewable-Tag (s/eq -|))
(s/def Offset-Tag (s/eq -+))
(s/def Scroll-Tag (s/eq -$))
(s/def Content s/Str)

(s/def ViewDefinition [(s/cond-pre Content Viewable-Tag Offset-Tag Scroll-Tag)])
(s/def HudDefinition [(s/cond-pre Content Viewable-Tag Offset-Tag Scroll-Tag ViewDefinition Input-Area-Tag)])

(s/def NReplProps {(s/optional-key :response) NReplResponse})
(s/def ContextProps {(s/optional-key :eval-history) [s/Str]})

(s/def HudProps
  {:input-area     [s/Str]
   :persisted-area [s/Str]
   :view-area      [s/Str]
   :hidden-area    [s/Str]
   :field-of-view  s/Int
   :view-offset    s/Int
   :scroll-offset  s/Int})

(s/defn parse :- HudProps
  [def :- HudDefinition]
  (loop [persisted []
         view      []
         offset    []
         input     []
         [c & cs] def
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
             [:input :-|] (recur persisted view offset input cs (inc fov) voff soff :view)
             [:view :-|] (recur persisted view offset input cs (inc fov) voff soff :view)
             [:view :-+] (recur persisted view offset input cs fov (inc voff) soff :offset)
             [:input :-+] (recur persisted view offset input cs fov (inc voff) soff :offset)
             [:offset :-+] (recur persisted view offset input cs fov (inc voff) soff :offset)
             [_ :-$] (recur persisted view offset input cs fov voff (inc soff) parsing)
             [:view _] (recur persisted (conj view c) offset (conj input c) cs fov voff soff :input)
             [:offset _] (recur persisted view (conj offset c) (conj input c) cs fov voff soff :input)
             :else (recur persisted view offset (conj input c) cs fov voff soff :input))))

(s/defn derive-view :- View
  "Derives a view based on a list of tagged strings.
   Behaves similarly to `derive-hud`, but derives a simple view.
   Ignores hud-specific information like the persisted area."
  [def :- ViewDefinition]
  (let [{:keys [input-area
                field-of-view
                view-offset
                scroll-offset]} (parse def)]
    (-> input-area
        (derive-text)
        (v/create-view field-of-view)
        (v/with-view-offset view-offset)
        (v/with-scroll-offset scroll-offset))))

(s/defn derive-hud :- Hud
  "Derives a hud from a declarative list of tagged strings.
   Interprets the strings to Text as per `derive-text`.
   Highlights are however fed to the hud highlights.

   Uses additional tags to derive a hud for those strings.
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
  [def :- HudDefinition]
  (let [{:keys [input-area
                persisted-area
                field-of-view
                view-offset
                scroll-offset]} (parse def)
        hud              (h/create-hud field-of-view)
        parsed-input     (derive-text input-area)
        parsed-persisted (if (empty? persisted-area)
                           []
                           [(derive-text persisted-area)])
        persisted-view   (-> hud
                             (h/persisted-view)
                             (v/enrich-with parsed-persisted)
                             (v/with-view-offset view-offset)
                             (v/with-scroll-offset scroll-offset))
        header           (->> hud
                              (h/persisted-view)
                              (v/text)
                              (:lines)
                              (mapv #(apply str %)))
        highlights       (-> (concat header persisted-area input-area)
                             (derive-text)
                             (:selection))]
    (as-> hud h
          (h/with-persisted-view h persisted-view)
          (h/with-input-area h parsed-input)
          (h/refresh-view h)
          (h/switch-current-view h (-> h
                                       (h/current-view)
                                       (v/with-view-offset view-offset)))
          (h/with-previous-view h (v/empty-view-with-size field-of-view))
          (if (some? highlights)
            (h/with-manual-highlight h (h/create-manual-highlight default-config highlights))
            h))))

(s/defn create-eval-history :- EvalHistory
  [evaluations :- [s/Str]
   limit :- s/Num]
  (let [history (eh/create-eval-history limit)]
    (->> evaluations
         (mapv (fn [eval] (derive-text [eval])))
         (reduce (fn [history' text] (eh/insert text history')) history))))

(s/defn derive-context :- Context
  ([def :- HudDefinition]
   (derive-context def {}))
  ([def :- HudDefinition
    props :- ContextProps]
   (let [eval-history (-> props (:eval-history []) (create-eval-history 5))
         hud          (derive-hud def)]
     (c/context-from hud eval-history))))

(s/defn process :- Context
  ([context :- Context
    events :- [Event]]
   (process context events {}))
  ([context :- Context
    events :- [Event]
    props :- NReplProps]
   (let [nrepl (nrepl-client (:response props terminating-response))]
     (reduce (fn [context' event]
               (c/process context' event default-config nrepl)) context events))))
