(ns omnia.view.render
  (:require [schema.core :as s]
            [omnia.view.terminal :as t]
            [omnia.text.core :as i]
            [omnia.repl.hud :as h]
            [omnia.repl.context :as c]
            [omnia.config.components.core :refer [UserHighlighting]]
            [omnia.config.components.text :refer [backgrounds coloured-element Style]]
            [omnia.text.highlighting :refer [fold -text]]
            [omnia.repl.context :refer [Context]]
            [omnia.util.schema :refer [Point Region]]
            [omnia.util.collection :refer [merge-culling map-vals reduce-idx]]
            [omnia.util.arithmetic :refer [--]]))

(def HighlightPattern
  {:current c/Highlights
   :former  c/Highlights
   :hud     h/Hud})

(s/def highlight-priority :- {c/HighlightType s/Int}
  {:selection    3
   :closed-paren 2
   :open-paren   1})

(s/def PrintInstruction
  {:line                        [Character]
   :at                          s/Int
   :terminal                    t/Terminal
   :scheme                      UserHighlighting
   :styles                      [Style]
   (s/optional-key :sub-region) Point
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

(s/defn print-line!
  [{line     :line
    y        :at
    padding  :padding
    terminal :terminal
    [xs xe]  :sub-region
    cs       :scheme
    styles   :styles} :- PrintInstruction]
  (letfn [(put! [ch x y emission]
            (let [fg (get cs (coloured-element emission))
                  bg (get cs backgrounds)]
              (t/put! terminal ch x y fg bg styles)))
          (pad! [x]
            (when padding
              (dotimes [offset padding]
                (put! \space (+ x offset) y -text))))
          (print-from! [x emission chars]
            (reduce-idx
              (fn [x' _ input]
                (put! input x' y emission)) x nil chars))
          (print! [x emission chars]
            (print-from! x emission chars)
            (+ x (count chars)))
          (print-sub! [x emission chars]
            (let [x' (+ x (count chars))]
              (cond
                (and (<= x xs) (>= x' xe)) (->> chars
                                                (drop (- xs x)) ;; subvecs
                                                (take (- xe xs)) ;; subvecs
                                                (print-from! xs emission))
                (and (<= x xs) (> x' xs))  (->> chars
                                                (drop (- xs x)) ;; subvecs
                                                (print-from! xs emission))
                (>= x' xe)                 (->> chars
                                                (take (- xe x)) ;; subvecs
                                                (print-from! x emission))
                (> x' xs)                  (->> chars
                                                (print-from! x emission))
                :else                      nil)
              x'))]
    (-> (if (and xs xe) print-sub! print!)
        (fold 0 line)
        (pad!))))

(s/defn prioritise :- [c/Highlight]
  [highlights :- c/Highlights]
  (->> highlights (sort-by (comp highlight-priority key)) (map second)))

(s/defn prioritised :- [c/Highlight]
  [ctx :- Context, pattern :- HighlightPattern]
  (let [current     (:current pattern)
        former      (:former pattern)
        preview-ov  (-> ctx (c/preview-hud) (h/view-offset))
        previous-ov (-> ctx (c/previous-hud) (h/view-offset))]
    (if (= preview-ov previous-ov)
      ;; additive-diff nils highlights that don't have a diff
      (prioritise (merge-culling additive-diff current former))
      (prioritise current))))

(s/defn highlight!
  "This thing should not do any selection extraction
   It should purely just do lookups and recursion over indexes"
  [ctx :- Context, pattern :- HighlightPattern]
  (let [hud        (:hud pattern)
        terminal   (c/terminal ctx)
        highlights (prioritised ctx pattern)]
    (doseq [{region :region
             scheme :scheme
             styles :styles} highlights]
      (let [selection (h/clip-selection hud region)
            seeker    (h/text hud)
            [xs ys]   (:start selection)]
        (->> selection
             (i/extract-for seeker)
             (:lines)
             (reduce-idx
               (fn [y x line]
                 (print-line! {:at         (h/project-y hud y)
                               :line       (i/line seeker [x y])
                               :terminal   terminal
                               :scheme     scheme
                               :styles     styles
                               :sub-region [x (-> line (count) (+ x))]})
                 0) ys xs))))))

(s/defn clean-highlights!
  [ctx :- Context]
  (let [highlights (c/highlights ctx)
        garbage    (c/garbage ctx)
        previous   (c/previous-hud ctx)]
    (highlight! ctx {:current garbage
                     :former  highlights
                     :hud     previous})))

(s/defn render-highlights!
  [ctx :- Context]
  (let [highlights (c/highlights ctx)
        garbage    (c/garbage ctx)
        preview    (c/preview-hud ctx)]
    (highlight! ctx {:current highlights
                     :former  garbage
                     :hud     preview})))

(s/defn set-position!
  [ctx :- Context]
  (let [terminal (c/terminal ctx)
        preview  (c/preview-hud ctx)
        [x y]    (h/project-hud-cursor preview)]
    (t/move! terminal x y)))

;; === Rendering strategies ===

(s/defn padding :- s/Int
  [current-line :- [Character]
   former-line  :- [Character]]
  (let [c (count current-line)
        f (count former-line)]
    (if (> f c) (- f c) 0)))

(s/defn total!
  [ctx :- Context]
  (let [terminal (c/terminal ctx)
        now      (-> ctx (c/preview-hud) (h/project-hud) (:lines))
        then     (-> ctx (c/previous-hud) (h/project-hud) (:lines))
        scheme   (-> ctx (c/configuration) (:syntax) (:standard))
        limit    (max (count now) (count then))]
    (dotimes [y limit]
      (let [a (nth now y nil)
            b (nth then y nil)]
        (print-line!
          {:at       y
           :line     a
           :padding  (padding a b)
           :terminal terminal
           :scheme   scheme
           :styles  []})))))

(s/defn diff!
  [ctx :- Context]
  (let [terminal (c/terminal ctx)
        preview  (c/preview-hud ctx)
        previous (c/previous-hud ctx)
        config   (c/configuration ctx)
        now      (-> preview (h/project-hud) (:lines))
        then     (-> previous (h/project-hud) (:lines))
        scheme   (-> config (:syntax) (:standard))
        limit    (max (count now) (count then))]
    (if (not= (h/view-offset preview) (h/view-offset previous))
      (total! ctx)
      (dotimes [y limit]
        (let [a (nth now y nil)
              b (nth then y nil)]
          (when (not= a b)
            (print-line! {:at       y
                          :line     a
                          :padding  (padding a b)
                          :terminal terminal
                          :scheme   scheme
                          :styles   []})))))))

(s/defn clear!
  [ctx :- Context]
  (-> ctx (c/terminal) (t/clear!)))

(s/defn nothing!
  [ctx :- Context]
  (let [preview-ov  (-> ctx (c/preview-hud) (h/view-offset))
        previous-ov (-> ctx (c/previous-hud) (h/view-offset))]
    (when (not= preview-ov previous-ov)
      (total! ctx))))

(s/defn refresh!
  [ctx :- Context]
  (-> ctx (c/terminal) (t/refresh!)))

(s/defn render!
  [ctx :- Context]
  (case (c/rendering ctx)
    :diff    (doto ctx (clean-highlights!) (diff!) (render-highlights!) (set-position!) (refresh!))
    :clear   (doto ctx (clear!) (total!) (set-position!) (refresh!))
    :nothing (doto ctx (clean-highlights!) (nothing!) (render-highlights!) (set-position!) (refresh!))
    (doto ctx (total!) (render-highlights!) (set-position!) (refresh!))))