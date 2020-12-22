(ns omnia.render
  (:require [omnia.terminal :as t]
            [omnia.input :as i]
            [omnia.hud :as h]
            [omnia.context :as c]
            [schema.core :as s]
            [omnia.highlight :refer [foldl -back ->text]]
            [omnia.context :refer [Context]]
            [omnia.more :refer [Point Region merge-common-with map-vals reduce-idx --]]))

(def HighlightPattern
  {:current c/Highlights
   :former  c/Highlights
   :hud     h/Hud})

(s/def highlight-priority :- {c/HighlightType s/Int}
  {:selection    3
   :closed-paren 2
   :open-paren   1})

(s/defn additive-diff :- (s/maybe Region)
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

(defn print-line! [{line     :line
                    y        :at
                    padding  :padding
                    terminal :terminal
                    [xs xe]  :sub-region
                    cs       :scheme
                    styles   :styles}]
  (letfn [(do-print! [ch x y state]
            (let [fg (cs (:id state))
                  bg (cs -back)]
              (t/put! terminal ch x y fg bg styles)))
          (pad! [x]
            (when padding
              (dotimes [offset padding]
                (do-print! \space (+ x offset) y ->text))))
          (show! [x emission state]
            (reduce-idx
              (fn [x' _ input]
                (do-print! input x' y state)) x nil emission))
          (print! [x [emission state]]
            (show! x emission state)
            (+ x (count emission)))
          (print-sub! [x [emission state]]
            (let [x' (+ x (count emission))]
              (cond
                (and (<= x xs) (>= x' xe)) (show! xs (->> emission (drop (- xs x)) (take (- xe xs))) state)
                (and (<= x xs) (> x' xs))  (show! xs (drop (- xs x) emission) state)
                (>= x' xe)                 (show! x (take (- xe x) emission) state)
                (> x' xs)                  (show! x emission state)
                :else nil)
              x'))]
    (-> (if (and xs xe) print-sub! print!)
        (foldl 0 line)
        (pad!))))

(s/defn prioritise :- c/Highlights
  [highlights :- c/Highlights]
  (if (:selection highlights)
    (select-keys highlights [:selection])
    highlights))

(s/defn prioritise' :- [c/Highlight]
  [highlights :- c/Highlights]
  (->> highlights (sort-by (comp highlight-priority key)) (map second)))

(s/defn prioritised :- [c/Highlight]
  [ctx :- Context, pattern :- HighlightPattern]
  (let [current     (:current pattern)
        former      (:former pattern)
        preview-ov  (-> ctx (c/preview-hud) (h/overview))
        previous-ov (-> ctx (c/previous-hud) (h/overview))]
    (if (= preview-ov previous-ov)
      ;; the additive diff immediately nils highlights that don't need rendering
      (->> (merge-common-with additive-diff current former) (prioritise') (remove nil?))
      (prioritise' current))))

(s/defn highlight!
  [ctx :- Context, pattern :- HighlightPattern]
  (let [hud        (:hud pattern)
        terminal   (c/terminal ctx)]
    (->> pattern
         (prioritised ctx)
         (run!
           (fn [{region :region scheme :scheme styles :styles}]
             (let [selection (h/project-selection hud region)
                   seeker    (h/text hud)
                   [xs ys]   (:start selection)]
               (->> (i/extract-for seeker selection)
                    (:lines)
                    (reduce-idx
                      (fn [y x line]
                        (print-line! {:at         (h/project-y hud y)
                                      :line       (i/line seeker [x y])
                                      :terminal   terminal
                                      :scheme     scheme
                                      :styles     styles
                                      :sub-region [x (+ x (count line))]})
                        0) ys xs))))))))

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
        [x y]    (h/project-cursor preview)]
    (t/move! terminal x y)))

;; === Rendering strategies ===

(defn- pad [current-line former-line]
  (let [c (count current-line)
        f (count former-line)]
    (when (> f c) (- f c))))

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
           :padding  (pad a b)
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
    (if (not= (h/overview preview) (h/overview previous))
      (total! ctx)
      (dotimes [y limit]
        (let [a (nth now y nil)
              b (nth then y nil)]
          (when (not= a b)
            (print-line! {:at       y
                          :line     a
                          :padding  (pad a b)
                          :terminal terminal
                          :scheme   scheme
                          :styles   []})))))))

(s/defn clear!
  [ctx :- Context]
  (-> ctx (c/terminal) (t/clear!)))

(s/defn nothing!
  [ctx :- Context]
  (let [preview-ov  (-> ctx (c/preview-hud) (h/overview))
        previous-ov (-> ctx (c/previous-hud) (h/overview))]
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