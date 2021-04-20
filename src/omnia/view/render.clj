(ns omnia.view.render
  (:require [schema.core :as s]
            [omnia.view.terminal :as t]
            [omnia.text.core :as i]
            [omnia.repl.hud :as h]
            [omnia.repl.context :as c]
            [omnia.config.components.core :refer [UserHighlighting]]
            [omnia.config.components.text :refer [backgrounds coloured-element Style ColouredElement]]
            [omnia.text.highlighting :refer [fold -text]]
            [omnia.repl.context :refer [Context]]
            [omnia.util.schema :refer [Point Region]]
            [omnia.util.collection :refer [merge-culling map-vals reduce-idx]])
  (:import (omnia.view.terminal Terminal)))

(def HighlightPattern
  {:current c/Highlights
   :former  c/Highlights
   :hud     h/Hud})

(s/def highlight-priority :- {c/HighlightType s/Int}
  {:selection    3
   :closed-paren 2
   :open-paren   1})

;(s/def PrintInstruction
;  {:line                        [Character]
;   :at                          s/Int
;   :terminal                    t/Terminal
;   :scheme                      UserHighlighting
;   :styles                      [Style]
;   (s/optional-key :sub-region) Point
;   (s/optional-key :padding)    s/Int})

(s/def PrintInstruction
  {:line                        [Character]
   :at                          s/Int
   :start                       s/Int
   :end                         s/Int
   :terminal                    t/Terminal
   :scheme                      UserHighlighting
   :styles                      [Style]
   (s/optional-key :padding)    s/Int})

(s/defn additive-diff :- (s/maybe c/Highlight)
  [current :- c/Highlight, former :- c/Highlight]
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

(s/defn prioritise :- [c/Highlight]
  [highlights :- c/Highlights]
  (->> highlights (sort-by (comp highlight-priority key)) (map #(nth % 1))))

(s/defn cull :- [c/Highlight]
  [ctx :- Context, current :- c/Highlight, former :- c/Highlight]
  (let [preview-ov  (-> ctx (c/preview-hud) (h/view-offset))
        previous-ov (-> ctx (c/previous-hud) (h/view-offset))]
    (if (= preview-ov previous-ov)
      ;; additive-diff nils highlights that don't have a diff
      (prioritise (merge-culling additive-diff current former))
      (prioritise current))))

(s/defn put!
  [terminal  :- Terminal
   character :- Character
   x         :- s/Int
   y         :- s/Int
   emission  :- ColouredElement
   scheme    :- UserHighlighting
   styles    :- [Style]]
  (let [fg (get scheme (coloured-element emission))
        bg (get scheme backgrounds)]
    (t/put! terminal character x y fg bg styles)))

(s/defn print-line!
  [terminal :- Terminal
   line     :- i/Line
   y        :- s/Int
   padding  :- s/Int
   scheme   :- UserHighlighting
   styles   :- [Style]]
  (let [pad!   (fn [x]
                 (dotimes [offset padding]
                   (put! terminal \space (+ x offset) y -text scheme styles)))
        print! (fn [x emission chars]
                 (reduce-idx
                   (fn [x' _ character]
                     (put! terminal character x' y emission scheme styles)) x nil chars)
                 (+ x (count chars)))]
    (->> line (fold print! 0) (pad!))))

(s/defn print-highlight!
  [terminal  :- Terminal
   hud       :- h/Hud
   highlight :- c/Highlight]
  (let [selection (->> highlight (:region) (h/clip-selection hud))
        scheme    (->> highlight (:scheme))
        styles    (->> highlight (:styles))
        seeker    (h/text hud)
        [xs ys]   (:start selection)
        [xe ye]   (:end selection)]
    (doseq [y (range ys (inc ye))]
      (let [line  (i/line-at seeker y)
            xs    (if (= y ys) xs 0)
            xe    (if (= y ye) xe (count line))
            y'    (h/project-y hud y)]
        (doseq [x' (range xs xe)]
          (put! terminal (nth line x') x' y' -text scheme styles))))))

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
            [_ ys] (:start region)
            [_ ye] (:end region)]
        (doseq [y (range ys (inc ye))]
          (print-line! terminal
                       (i/line-at text y)
                       (h/project-y preview y)
                       0
                       scheme
                       styles))))))

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
  [terminal :- Terminal,
   ctx      :- Context]
  (let [now      (-> ctx (c/preview-hud) (h/project-hud) (:lines))
        then     (-> ctx (c/previous-hud) (h/project-hud) (:lines))
        scheme   (-> ctx (c/configuration) (:syntax) (:standard))
        limit    (max (count now) (count then))]
    (dotimes [y limit]
      (let [a   (nth now y nil)
            b   (nth then y nil)
            pad (padding a b)]
        (print-line! terminal a y pad scheme [])))))

(s/defn render-diff!
  [terminal :- Terminal
   ctx      :- Context]
  (let [preview  (c/preview-hud ctx)
        previous (c/previous-hud ctx)
        config   (c/configuration ctx)
        now      (-> preview (h/project-hud) (:lines))
        then     (-> previous (h/project-hud) (:lines))
        scheme   (-> config (:syntax) (:standard))
        limit    (max (count now) (count then))]
    (if (not= (h/view-offset preview) (h/view-offset previous))
      (render-total! terminal ctx)
      (dotimes [y limit]
        (let [a   (nth now y nil)
              b   (nth then y nil)
              pad (padding a b)]
          (when (not= a b)
            (print-line! terminal a y pad scheme [])))))))

(s/defn render-nothing!
  [terminal :- Terminal,
   ctx      :- Context]
  (let [preview-ov  (-> ctx (c/preview-hud) (h/view-offset))
        previous-ov (-> ctx (c/previous-hud) (h/view-offset))]
    (when (not= preview-ov previous-ov)
      (render-total! terminal ctx))))

(s/defn render!
  [ctx      :- Context,
   terminal :- Terminal]
  (case (c/rendering ctx)
    :diff (doto terminal (clean-highlights! ctx) (render-diff! ctx) (render-highlights! ctx) (set-position! ctx) (t/refresh!))
    :clear (doto terminal (t/clear!) (render-total! ctx) (set-position! ctx) (t/refresh!))
    :nothing (doto terminal (clean-highlights! ctx) (render-nothing! ctx) (render-highlights! ctx) (set-position! ctx) (t/refresh!))
    (doto terminal (render-total! ctx) (render-highlights! ctx) (set-position! ctx) (t/refresh!))))