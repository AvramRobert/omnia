(ns omnia.render
  (:require [omnia.terminal :as t]
            [omnia.input :as i]
            [omnia.highlight :as h]
            [omnia.more :refer [map-vals reduce-idx --]]
            [clojure.core.match :as m]))

(declare total! diff! nothing!)

;; === Highlighting scheme ==

(def primary 1)
(def secondary 0)

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
        top (top-y hud)
        bottom (bottom-y hud)
        {[xs ys] :start
         [xe ye] :end} selection
        unpaged? (< h fov)
        clipped-top? (< ys top)
        clipped-bottom? (> ye bottom)
        visible-top? (<= top ys bottom)
        visible-bottom? (<= top ye bottom)
        end-bottom (-> hud (i/reset-y bottom) (i/end-x) (:cursor))]
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
        (i/extract)
        (assoc :height (:height hud)))))                    ;; for accurate projection

(defn additive-diff [current formers]
  (if-let [former (some #(when (= (:type current) (:type %)) %) formers)]
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
        :else current))
    current))

(defn- display! [emission terminal x y]
  (reduce-idx (fn [ix _ input] (t/put! terminal input ix y)) x nil emission))

(defn print-line! [{:keys [line
                           scheme
                           state
                           coordinate
                           terminal]
                     :or {state h/->break}}]
  (let [[x y] coordinate
        ix   (atom x)
        {cs :cs style :style} scheme]
    (t/background! terminal (cs h/-back))
    (when style (t/style! terminal style))
    (t/visible! terminal false)
    (h/process-from! line state
       (fn [emission type]
         (t/foreground! terminal (cs type))
         (display! emission terminal @ix y)
         (swap! ix #(+ % (count emission)))))
    (t/background! terminal :default)
    (t/visible! terminal true)
    (when style (t/un-style! terminal style))))

(defn highlight! [ctx highlights]
  (let [{terminal :terminal
         complete :complete-hud} ctx]
    (doseq [{highlight :region
             scheme    :scheme} highlights
            :let [selection (project-selection complete highlight)
                  [xs ys]   (:start selection)
                  hud       (region complete selection)
                  state     (h/state-at (first (:lines hud)) xs)]]
      (->> (:lines hud)
           (reduce-idx
             (fn [y x line]
               (print-line! {:line     line
                             :terminal terminal
                             :scheme   scheme
                             :state    state
                             :coordinate [x (project-y hud y)]})
               0) ys xs)))))

(defn clean! [{:keys [colourscheme highlights] :as ctx}]
  ;; Always re-render from the beginning of the line to avoid syntax highlighting artifacts
  (letfn [(reset [selection]
            (some-> selection #_(additive-diff selection highlights)
                    (update-in [:region :start] (fn [[_ y]] [0 y]))
                    (assoc :scheme (-> colourscheme (clean-cs) (simple-scheme)))))]
    (->> (:garbage ctx) (mapv reset) (remove nil?) (highlight! ctx))))

(defn selections! [{:keys [highlights garbage] :as ctx}]
  (->> highlights
       (sort-by :priority)
       #_(mapv #(additive-diff % garbage))
       (remove nil?)
       (highlight! ctx)))

(defn position! [{:keys [terminal complete-hud]}]
  (let [[x y] (project-cursor complete-hud)]
    (t/move! terminal x y)))

;; === Rendering strategies ===

;; FIXME: Merge padding with printing
(defn- pad-erase [current-line former-line]
  (cond
    (and (empty? former-line)
         (empty? current-line)) []
    (empty? former-line)  current-line
    (empty? current-line) (-> (count former-line) (repeat \space) (vec))
    :else (let [hc (count current-line)
                hf (count former-line)
                largest (max hc hf)]
            (->> (range 0 (- largest hc))
                 (reduce (fn [c _] (conj c \space)) current-line)))))

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
            (print-line! {:line      (pad-erase a b)
                          :terminal   terminal
                          :scheme     scheme
                          :coordinate [0 y]})))))))

(defn nothing! [ctx]
  (let [{complete :complete-hud
         previous :previous-hud} ctx]
    (when (not= (:ov complete) (:ov previous))
      (total! ctx))))

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
          {:line       (pad-erase a b)
           :terminal   terminal
           :scheme     scheme
           :coordinate [0 y]})))))

(defn render [ctx]
  (case (:render ctx)
    :diff (doto ctx (clean!) (diff!) (selections!) (position!))
    :nothing (doto ctx (clean!) (nothing!) (selections!) (position!))
    (doto ctx (total!) (selections!) (position!))))