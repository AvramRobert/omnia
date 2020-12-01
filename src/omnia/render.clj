(ns omnia.render
  (:require [omnia.terminal :as t]
            [omnia.input :as i]
            [omnia.hud :as h]
            [schema.core :as s]
            [omnia.highlight :refer [foldl -back ->text]]
            [omnia.more :refer [Point Region lmerge-with map-vals reduce-idx --]]))

(defn additive-diff [current former]
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

(defn prioritise [highlights]
  (if (:selection highlights)
    (select-keys highlights [:selection])
    highlights))

(defn highlight! [ctx {:keys [current former hud]}]
  (let [{complete :complete-hud
         previous :previous-hud
         terminal :terminal} ctx
        highlights (if (= (:ov complete) (:ov previous))
                     (lmerge-with additive-diff (prioritise current) (prioritise former))
                     (prioritise current))]
    (->> (vals highlights)
         (remove nil?)
         (run!
           (fn [{region :region scheme :scheme styles :styles}]
             (let [selection (h/project-selection hud region)
                   seeker    (:seeker hud)
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

(defn clean-highlights! [{:keys [garbage
                                 highlights
                                 previous-hud] :as ctx}]
  (highlight! ctx {:current garbage
                   :former  highlights
                   :hud     previous-hud}))

(defn render-highlights! [{:keys [highlights
                                  garbage
                                  complete-hud] :as ctx}]
  (highlight! ctx {:current highlights
                   :former  garbage
                   :hud     complete-hud}))

(defn set-position! [{:keys [terminal complete-hud]}]
  (let [[x y] (h/project-cursor complete-hud)]
    (t/move! terminal x y)))

;; === Rendering strategies ===

(defn- pad [current-line former-line]
  (let [c (count current-line)
        f (count former-line)]
    (when (> f c) (- f c))))

(defn total! [ctx]
  (let [terminal (:terminal ctx)
        now      (-> ctx (:complete-hud) (h/project-hud) (:lines))
        then     (-> ctx (:previous-hud) (h/project-hud) (:lines))
        scheme   (-> ctx (:config) (:syntax) (:standard))
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

(defn render-diff! [ctx]
  (let [{terminal     :terminal
         complete     :complete-hud
         previous     :previous-hud
         config       :config} ctx
        now      (-> complete (h/project-hud) (:lines))
        then     (-> previous (h/project-hud) (:lines))
        scheme   (-> config (:syntax) (:standard))
        limit    (max (count now) (count then))]
    (if (not= (:ov complete) (:ov previous))
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

(defn clear! [ctx]
  (-> ctx (:terminal) (t/clear!)))

(defn nothing! [ctx]
  (let [{complete :complete-hud
         previous :previous-hud} ctx]
    (when (not= (:ov complete) (:ov previous))
      (total! ctx))))

(defn refresh! [ctx]
  (-> ctx (:terminal) (t/refresh!)))

(defn render! [ctx]
  (case (:render ctx)
    :diff (doto ctx (clean-highlights!) (render-diff!) (render-highlights!) (set-position!) (refresh!))
    :clear (doto ctx (clear!) (total!) (set-position!) (refresh!))
    :nothing (doto ctx (clean-highlights!) (nothing!) (render-highlights!) (set-position!) (refresh!))
    (doto ctx (total!) (render-highlights!) (set-position!) (refresh!))))