(ns omnia.display.core
  (:require [schema.core :as s]
            [omnia.display.terminal :as t]
            [omnia.repl.text :as i]
            [omnia.repl.view :as v]
            [omnia.repl.hud :as h]
            [omnia.repl.context :as c]
            [omnia.repl.syntax-highlighting :as st]
            [omnia.util.collection :as uc]
            [omnia.schema.render :refer [HighlightInstructions HighlightInstructionType HighlightInstructionData]]
            [omnia.schema.config :refer [Highlighting]]
            [omnia.schema.text :refer [Line]]
            [omnia.schema.view :refer [View]]
            [omnia.schema.syntax :refer [Style SyntaxElement texts backgrounds]]
            [omnia.schema.context :refer [Context]]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.terminal :refer [Terminal]]))

(s/def highlighting-priority :- {HighlightInstructionType s/Int}
  {:selection    3
   :closed-paren 2
   :open-paren   1
   :manual       0})

(s/defn additive-diff :- (s/maybe HighlightInstructionData)
  [current :- HighlightInstructionData, former :- HighlightInstructionData]
  (let [{[xs ys] :from
         [xe ye] :until} (:region current)
        {[xs' ys'] :from
         [xe' ye'] :until} (:region former)
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
          (and exact-start? grown-bottom?)) (assoc current :region {:from [xe' ye'] :until [xe ye]})

      (or (and exact-end? similar-start? grown-left?)
          (and exact-end? grown-top?)) (assoc current :region {:from [xs  ys] :until [xs' ys']})
      :else current)))

(s/defn prioritise :- [HighlightInstructionData]
  [instructions :- HighlightInstructions]
  (->> instructions (sort-by (comp highlighting-priority key)) (map #(nth % 1))))

(s/defn cull :- [HighlightInstructionData]
  "Diffs the `current` highlight against the `former` highlight.
  Returns only the diffed region from the `current` that doesn't overlap
  with anything in the `former`."
  [hud :- Hud, current :- HighlightInstructions, former :- HighlightInstructions]
  (let [preview-ov  (-> hud (h/current-view) (v/view-offset))
        previous-ov (-> hud (h/previous-view) (v/view-offset))]
    (if (= preview-ov previous-ov)
      ;; a nil from additive-diff means the highlights that don't have a diff
      (prioritise (uc/merge-common-with additive-diff current former))
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
                 (uc/reduce-idx
                   (fn [x' _ character]
                     (when (and (>= x' xs) (< x' xe))
                       (put-char! terminal character x' y emission scheme styles))) x nil chars)
                 (+ x (count chars)))]
    (->> line (st/fold print! 0) (pad!))))

(s/defn print-highlight!
  [terminal  :- Terminal
   view       :- View
   highlight :- HighlightInstructionData]
  (let [selection (->> highlight (:region) (v/clip-selection view))
        scheme    (->> highlight (:scheme))
        styles    (->> highlight (:styles))
        text      (v/text view)
        [xs ys]   (:from selection)
        [xe ye]   (:until selection)]
    (doseq [y (range ys (inc ye))]
      (let [line  (i/line-at text y)
            xs    (if (= y ys) xs 0)
            xe    (if (= y ye) xe (count line))
            y'    (v/project-y view y)]
        (doseq [x' (range xs xe)]
          (put-char! terminal (nth line x') x' y' texts scheme styles))))))

(s/defn clean-highlights!
  "Note: Cleaning re-prints the entire line to restore syntax highlighting"
  [terminal :- Terminal,
   hud      :- Hud]
  (let [highlights (h/highlights hud)
        garbage    (h/garbage hud)
        preview    (h/current-view hud)
        text       (v/text preview)]
    (doseq [highlight (cull hud garbage highlights)]
      (let [region (->> highlight (:region) (v/clip-selection preview))
            scheme (->> highlight (:scheme))
            styles (->> highlight (:styles))
            [xs ys] (:from region)
            [xe ye] (:until region)]
        (doseq [y (range ys (inc ye))
                :let [line (i/line-at text y)
                      xs   (if (= y ys) xs 0)
                      xe   (if (= y ye) xe (count line))
                      y    (v/project-y preview y)]]
          (print-line! terminal line y xs xe 0 scheme styles))))))

(s/defn render-highlights!
  [terminal :- Terminal,
   hud      :- Hud]
  (let [highlights (h/highlights hud)
        garbage    (h/garbage hud)
        preview    (h/current-view hud)]
    (doseq [highlight (cull hud highlights garbage)]
      (print-highlight! terminal preview highlight))))

(s/defn set-position!
  [terminal :- Terminal
   hud      :- Hud]
  (let [preview  (h/current-view hud)
        [x y]    (v/project-view-cursor preview)]
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
   hud      :- Hud]
  (let [now      (-> hud (h/current-view) (v/project-view-text))
        then     (-> hud (h/previous-view) (v/project-view-text))
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
   hud      :- Hud]
  (let [preview  (h/current-view hud)
        previous (h/previous-view hud)
        now      (v/project-view-text preview)
        then     (v/project-view-text previous)
        scheme   (-> config (:syntax) (:standard))
        limit    (max (count now) (count then))]
    (if (not= (v/view-offset preview) (v/view-offset previous))
      (render-total! terminal config hud)
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
   hud      :- Hud]
  (let [preview-ov  (-> hud (h/current-view) (v/view-offset))
        previous-ov (-> hud (h/previous-view) (v/view-offset))]
    (when (not= preview-ov previous-ov)
      (render-total! terminal config hud))))

(s/defn render!
  [context  :- Context
   config   :- Config
   terminal :- Terminal]
  (let [hud (c/hud context)]
    (case (h/rendering hud)
      :diff (doto terminal (clean-highlights! hud) (render-diff! config hud) (render-highlights! hud) (set-position! hud) (t/refresh!))
      :clear (doto terminal (t/clear!) (render-total! config hud) (set-position! hud) (t/refresh!))
      :nothing (doto terminal (clean-highlights! hud) (render-nothing! config hud) (render-highlights! hud) (set-position! hud) (t/refresh!))
      (doto terminal (render-total! config hud) (render-highlights! hud) (set-position! hud) (t/refresh!)))))
