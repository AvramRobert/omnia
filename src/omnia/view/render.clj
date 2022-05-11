(ns omnia.view.render
  (:require [schema.core :as s]
            [omnia.view.terminal :as t]
            [omnia.repl.text :as i]
            [omnia.repl.hud :as h]
            [omnia.repl.context :as c]
            [omnia.repl.syntax-highlighting :as st]
            [omnia.util.collection :refer [merge-common-with reduce-idx]]
            [omnia.schema.render :refer [HighlightInstructions HighlightInstructionType HighlightInstructionData]]
            [omnia.schema.config :refer [Highlighting]]
            [omnia.schema.text :refer [Line]]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.syntax :refer [Style SyntaxElement texts backgrounds]]
            [omnia.schema.context :refer [Context]]
            [omnia.schema.config :refer [Config]])
  (:import (omnia.view.terminal Terminal)))

(s/def highlighting-priority :- {HighlightInstructionType s/Int}
  {:selection    3
   :closed-paren 2
   :open-paren   1
   :manual       0})

(s/defn additive-diff :- (s/maybe HighlightInstructionData)
  [current :- HighlightInstructionData, former :- HighlightInstructionData]
  (let [{[xs ys]   :start
         [xe ye]   :end} (:region current)
        {[xs' ys'] :start
         [xe' ye'] :end} (:region former)
        exact-start?   (= [xs ys] [xs' ys'])
        exact-end?     (= [xe ye] [xe' ye'])
        similar-start? (= ys ys')
        similar-end?   (= ye ye')
        shrunk-left?   (> xs xs')
        shrunk-right?  (< xe xe')
        shrunk-top?    (> ys ys')
        shrunk-bottom? (< ye ye')
        grown-left?    (< xs xs')
        grown-right?   (> xe xe')
        grown-top?     (< ys ys')
        grown-bottom?  (> ye ye')]
    (cond
      (or (and exact-start? exact-end?)
          (and exact-start? similar-end? shrunk-right?)
          (and exact-start? shrunk-bottom?)
          (and exact-end? similar-start? shrunk-left?)
          (and exact-end? shrunk-top?)) nil

      (or (and exact-start? similar-end? grown-right?)
          (and exact-start? grown-bottom?)) (assoc current :region {:start [xe' ye'] :end [xe ye]})

      (or (and exact-end? similar-start? grown-left?)
          (and exact-end? grown-top?)) (assoc current :region {:start [xs  ys] :end [xs' ys']})
      :else current)))

(s/defn prioritise :- [HighlightInstructionData]
  [instructions :- HighlightInstructions]
  (->> instructions (sort-by (comp highlighting-priority key)) (map #(nth % 1))))

(s/defn cull :- [HighlightInstructionData]
  "Diffs the `current` highlight against the `former` highlight.
  Returns only the diffed region from the `current` that doesn't overlap
  with anything in the `former`."
  [ctx :- Context, current :- HighlightInstructions, former :- HighlightInstructions]
  (let [preview-ov  (-> ctx (c/preview-hud) (h/view-offset))
        previous-ov (-> ctx (c/previous-hud) (h/view-offset))]
    (if (= preview-ov previous-ov)
      ;; a nil from additive-diff means the highlights that don't have a diff
      (prioritise (merge-common-with additive-diff current former))
      (prioritise current))))

(s/defn put-char!
  [terminal  :- Terminal
   character :- Character
   x         :- s/Int
   y         :- s/Int
   emission  :- SyntaxElement
   scheme    :- Highlighting
   styles    :- [Style]]
  (let [fg (get scheme emission)
        bg (get scheme backgrounds)]
    (t/put! terminal character x y fg bg styles)))

(s/defn print-line!
  "Prints a line with syntax highlighting and styles.
   To keep syntax highlighting correct, iterates through the whole line.
   It however explicitly prints only the sub-regions specified by [xs xe]."
  [terminal :- Terminal
   line     :- Line
   y        :- s/Int
   xs       :- s/Int
   xe       :- s/Int
   padding  :- s/Int
   scheme   :- Highlighting
   styles   :- [Style]]
  (let [pad!   (fn [x]
                 (dotimes [offset padding]
                   (put-char! terminal \space (+ x offset) y texts scheme styles)))
        print! (fn [x emission chars]
                 (reduce-idx
                   (fn [x' _ character]
                     (when (and (>= x' xs) (< x' xe))
                       (put-char! terminal character x' y emission scheme styles))) x nil chars)
                 (+ x (count chars)))]
    (->> line (st/fold print! 0) (pad!))))

(s/defn print-highlight!
  [terminal  :- Terminal
   hud       :- Hud
   highlight :- HighlightInstructionData]
  (let [selection (->> highlight (:region) (h/clip-selection hud))
        scheme    (->> highlight (:scheme))
        styles    (->> highlight (:styles))
        text    (h/text hud)
        [xs ys]   (:start selection)
        [xe ye]   (:end selection)]
    (doseq [y (range ys (inc ye))]
      (let [line  (i/line-at text y)
            xs    (if (= y ys) xs 0)
            xe    (if (= y ye) xe (count line))
            y'    (h/project-y hud y)]
        (doseq [x' (range xs xe)]
          (put-char! terminal (nth line x') x' y' texts scheme styles))))))

(s/defn clean-highlights!
  "Note: Cleaning re-prints the entire line to restore syntax highlighting"
  [terminal :- Terminal,
   ctx      :- Context]
  (let [highlights (c/highlights ctx)
        garbage    (c/garbage ctx)
        preview    (c/preview-hud ctx)
        text       (h/text preview)]
    (doseq [highlight (cull ctx garbage highlights)]
      (let [region (->> highlight (:region) (h/clip-selection preview))
            scheme (->> highlight (:scheme))
            styles (->> highlight (:styles))
            [xs ys] (:start region)
            [xe ye] (:end region)]
        (doseq [y (range ys (inc ye))
                :let [line (i/line-at text y)
                      xs   (if (= y ys) xs 0)
                      xe   (if (= y ye) xe (count line))
                      y    (h/project-y preview y)]]
          (print-line! terminal line y xs xe 0 scheme styles))))))

(s/defn render-highlights!
  [terminal :- Terminal,
   ctx      :- Context]
  (let [highlights (c/highlights ctx)
        garbage    (c/garbage ctx)
        preview    (c/preview-hud ctx)]
    (doseq [highlight (cull ctx highlights garbage)]
      (print-highlight! terminal preview highlight))))

(s/defn set-position!
  [terminal :- Terminal
   ctx      :- Context]
  (let [preview  (c/preview-hud ctx)
        [x y]    (h/project-hud-cursor preview)]
    (t/move! terminal x y)))

;; === Rendering strategies ===

(s/defn padding :- s/Int
  [current-line :- [Character]
   former-line  :- [Character]]
  (let [c (count current-line)
        f (count former-line)]
    (if (> f c) (- f c) 0)))

(s/defn render-total!
  [terminal :- Terminal
   config   :- Config
   ctx      :- Context]
  (let [now      (-> ctx (c/preview-hud) (h/project-hud-text))
        then     (-> ctx (c/previous-hud) (h/project-hud-text))
        scheme   (-> config (:syntax) (:standard))
        limit    (max (count now) (count then))]
    (dotimes [y limit]
      (let [line      (nth now y nil)
            line-then (nth then y nil)
            pad       (padding line line-then)
            xs        0
            xe        (count line)]
        (print-line! terminal line y xs xe pad scheme [])))))

(s/defn render-diff!
  [terminal :- Terminal
   config   :- Config
   ctx      :- Context]
  (let [preview  (c/preview-hud ctx)
        previous (c/previous-hud ctx)
        now      (h/project-hud-text preview)
        then     (h/project-hud-text previous)
        scheme   (-> config (:syntax) (:standard))
        limit    (max (count now) (count then))]
    (if (not= (h/view-offset preview) (h/view-offset previous))
      (render-total! terminal config ctx)
      (dotimes [y limit]
        (let [line      (nth now y nil)
              line-then (nth then y nil)
              pad       (padding line line-then)
              xs        0
              xe        (count line)]
          (when (not= line line-then)
            (print-line! terminal line y xs xe pad scheme [])))))))

(s/defn render-nothing!
  [terminal :- Terminal
   config   :- Config
   ctx      :- Context]
  (let [preview-ov  (-> ctx (c/preview-hud) (h/view-offset))
        previous-ov (-> ctx (c/previous-hud) (h/view-offset))]
    (when (not= preview-ov previous-ov)
      (render-total! terminal config ctx))))

(s/defn render!
  [ctx      :- Context
   config   :- Config
   terminal :- Terminal]
  (case (c/rendering ctx)
    :diff (doto terminal (clean-highlights! ctx) (render-diff! config ctx) (render-highlights! ctx) (set-position! ctx) (t/refresh!))
    :clear (doto terminal (t/clear!) (render-total! config ctx) (set-position! ctx) (t/refresh!))
    :nothing (doto terminal (clean-highlights! ctx) (render-nothing! config ctx) (render-highlights! ctx) (set-position! ctx) (t/refresh!))
    (doto terminal (render-total! config ctx) (render-highlights! ctx) (set-position! ctx) (t/refresh!))))
