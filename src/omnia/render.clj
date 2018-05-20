(ns omnia.render
  (:require [omnia.terminal :as t]
            [omnia.input :as i]
            [omnia.highlight :as h]
            [omnia.more :refer [lmerge-with map-vals reduce-idx --]]))

;; === Highlighting scheme ==

(defn simple-scheme [cs]
  {:cs cs :style nil})

;; === Various colourschemes ===

(defn no-cs [cs]
  (-> (cs h/-text :white)
      (constantly)
      (map-vals cs)))

(defn clean-cs [cs]
  (assoc cs h/-select :default
            h/-back   :default))

(defn select-cs [cs]
  (-> (no-cs cs)
      (assoc h/-back   (cs h/-select))
      (assoc h/-select (cs h/-select))))


;; === Projections ===

(defn bottom-y [hud]
  "The lower y bound of a page (exclusive)
  bottom-y = height - ov - 1
  Subtract 1 because we count from 0"
  (let [{height :height
         ov     :ov} hud]
    (-- height ov 1)))

(defn top-y [hud]
  "The upper y bound of a page (inclusive)
  top-y = (height - fov - ov)"
  (let [{height :height
         fov    :fov
         ov     :ov} hud]
    (-- height fov ov)))

(defn project-y [hud y]
  "given hud-y, screen-y = hud-y - top-y
   given screen-y, hud-y = screen-y + top-y"
  (let [{fov :fov
         h   :height} hud
        ys (top-y hud)]
    (if (> h fov) (- y ys) y)))

(defn project-cursor [hud]
  (let [[x hy] (:cursor hud)
        y (project-y hud hy)]
    [x y]))

(defn project-selection [hud selection]
  (let [{fov :fov
         h   :height} hud
        {[xs ys] :start
         [xe ye] :end} selection
        top             (top-y hud)
        bottom          (bottom-y hud)
        unpaged?        (< h fov)
        clipped-top?    (< ys top)
        clipped-bottom? (> ye bottom)
        visible-top?    (<= top ys bottom)
        visible-bottom? (<= top ye bottom)
        end-bottom      (-> hud (i/reset-y bottom) (i/end-x) (:cursor))]
    (cond
      unpaged? selection
      (and visible-top?
           visible-bottom?) selection
      (and visible-top?
           clipped-bottom?) {:start [xs ys]
                             :end   end-bottom}
      (and visible-bottom?
           clipped-top?) {:start [0 top]
                          :end   [xe ye]}
      :else {:start [0 bottom]
             :end   [0 bottom]})))

(defn project-hud [hud]
  (let [{lor     :lor
         fov     :fov
         ov      :ov
         scroll? :scroll?} hud
        take-right (fn [n coll] (lazy-seq (take-last n coll)))]
    (if scroll?
      (i/rebase hud #(->> % (take-right lor) (take fov)))
      (i/rebase hud #(->> % (drop-last ov) (take-right fov))))))

;; === Rendering primitives ===

(defn- region [hud selection]
  (let [{[xs ys] :start
         [xe ye] :end} selection]
    (-> hud
        (i/reset-y ys)
        (i/reset-x xs)
        (i/select)
        (i/reset-y ye)
        (i/reset-x xe)
        (i/extract))))

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
                    {cs    :cs
                     style :style} :scheme}]
  (letfn [(pad! [x]
            (when padding
              (dotimes [offset padding]
                (t/put! terminal \space (+ x offset) y))))
          (show! [x emission state]
            (t/foreground! terminal (cs (:id state)))
            (reduce-idx
              (fn [x' _ input]
                (t/put! terminal input x' y)) x nil emission))
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
    (t/background! terminal (cs h/-back))
    (when style (t/style! terminal style))
    (t/visible! terminal false)
    (-> (if (and xs xe) print-sub! print!)
        (h/foldl 0 line)
        (pad!))
    (t/background! terminal :default)
    (t/visible! terminal true)
    (when style (t/un-style! terminal style))))

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
           (fn [{highlight :region scheme :scheme}]
             (let [selection (project-selection hud highlight)
                   [xs ys]   (:start selection)]
               (->> (region hud selection)
                    (:lines)
                    (reduce-idx
                      (fn [y x line]
                        (print-line! {:at         (project-y hud y)
                                      :line       (i/line hud [x y])
                                      :terminal   terminal
                                      :scheme     scheme
                                      :sub-region [x (+ x (count line))]})
                        0) ys xs))))))))

(defn collect! [{:keys [garbage
                        highlights
                        previous-hud] :as ctx}]
  (highlight! ctx {:current garbage
                   :former  highlights
                   :hud     previous-hud}))

(defn selections! [{:keys [highlights
                           garbage
                           complete-hud] :as ctx}]
  (highlight! ctx {:current highlights
                   :former  garbage
                   :hud     complete-hud}))

(defn position! [{:keys [terminal complete-hud]}]
  (let [[x y] (project-cursor complete-hud)]
    (t/move! terminal x y)))

;; === Rendering strategies ===

(defn- pad [current-line former-line]
  (let [c (count current-line)
        f (count former-line)]
    (when (> f c) (- f c))))

(defn total! [ctx]
  (let [terminal (:terminal ctx)
        now      (-> ctx (:complete-hud) (project-hud) (:lines))
        then     (-> ctx (:previous-hud) (project-hud) (:lines))
        scheme   (-> ctx (:colourscheme) (simple-scheme))
        limit    (max (count now) (count then))]
    (dotimes [y limit]
      (let [a (nth now y nil)
            b (nth then y nil)]
        (print-line!
          {:at         y
           :line       a
           :padding    (pad a b)
           :terminal   terminal
           :scheme     scheme})))))

(defn diff! [ctx]
  (let [{terminal     :terminal
         complete     :complete-hud
         previous     :previous-hud
         colourscheme :colourscheme} ctx
        now      (-> complete (project-hud) (:lines))
        then     (-> previous (project-hud) (:lines))
        scheme   (simple-scheme colourscheme)
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
                          :scheme   scheme})))))))

(defn clear! [ctx]
  (-> ctx (:terminal) (t/clear!)))

(defn nothing! [ctx]
  (let [{complete :complete-hud
         previous :previous-hud} ctx]
    (when (not= (:ov complete) (:ov previous))
      (total! ctx))))

(defn render [ctx]
  (case (:render ctx)
    :diff (doto ctx (collect!) (diff!) (selections!) (position!))
    :clear (doto ctx (clear!) (total!) (position!))
    :nothing (doto ctx (collect!) (nothing!) (selections!) (position!))
    (doto ctx (total!) (selections!) (position!))))