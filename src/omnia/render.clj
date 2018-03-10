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

(defn common-region [ra rb]
  (let [{[xs ys] :start
         [xe ye] :end} ra
        {[xss yss] :start
         [xee yee] :end} rb]
    (when (not (or (> yss ye) (> ys yee)))
      {:start (if (> ys yss) [xs ys] [xss yss])
       :end   (if (< ye yee) [xe ye] [xee yee])})))

(defn diff-region [r c]
  (let [{[xs ys] :start
         [xe ye] :end} r
        {[xss yss] :start
         [xee yee] :end} c]
    (if (< ys yss)
      {:start [xs ys]
       :end   [xss yss]}
      {:start [xee yee]
       :end   [xe ye]})))

(defn occlude [new old]
  (letfn [(minimise [[t ha]]
            (->> (old t ha)
                 (common-region ha)
                 (diff-region ha)))]
    (->> new (mapv minimise) (into {}))))

(defn- display! [emission terminal x y]
  (reduce-idx (fn [ix _ input] (t/put! terminal input ix y)) x nil emission))

(defn print-line! [line terminal scheme [x y]]
  (let [ix (atom x)
        {cs :cs style :style} scheme]
    (t/background! terminal (cs h/-back))
    (when style (t/style! terminal style))
    (t/visible! terminal false)
    (h/process line
               (fn [emission type]
                 (t/foreground! terminal (cs type))
                 (display! emission terminal @ix y)
                 (swap! ix #(+ % (count emission)))))
    (t/background! terminal :default)
    (t/visible! terminal true)
    (when style (t/un-style! terminal style))))

(defn print-hud!
  ([hud terminal scheme]
   (print-hud! hud terminal scheme [0 0]))
  ([hud terminal scheme [x y]]
   (reduce-idx
     (fn [cy cx line]
       (print-line! line terminal scheme [cx (project-y hud cy)])
       0) y x (:lines hud))))

(defn highlight! [ctx regions]
  (let [{terminal :terminal
         complete :complete-hud} ctx]
    (run!
      #(let [scheme     (:scheme %)
             projection (project-selection complete %)
             {[xs ys] :start} projection]
         (-> (region complete projection)
             (print-hud! terminal scheme [xs ys]))) regions)))

(defn clean! [{:keys [colourscheme highlights] :as ctx}]
  ;; Always re-render from the beginning of the line to avoid syntax highlighting artifacts
  (letfn [(reset [selection]
            (-> selection
                ;(occlude highlights)                        ;; expects typed
                (update :start (fn [[_ y]] [0 y]))
                (assoc  :scheme (-> colourscheme (clean-cs) (simple-scheme)))))]
    (as-> ctx context
          (update context :garbage #(mapv reset %))
          (highlight! context (:garbage context)))))

(defn selections! [ctx]
  (->> (:highlights ctx)
       (sort-by :priority)
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
      (loop [y 0]
        (when (< y limit)
          (let [a (nth now y nil)
                b (nth then y nil)]
            (when (not= a b)
              (-> (pad-erase a b)
                  (print-line! terminal scheme [0 y])))
            (recur (inc y))))))))

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
    (loop [y 0]
      (when (< y limit)
        (let [a (nth now y nil)
              b (nth then y nil)]
          (-> (pad-erase a b)
              (print-line! terminal scheme [0 y]))
          (recur (inc y)))))))

(defn render [ctx]
  (case (:render ctx)
    :diff (doto ctx (clean!) (diff!) (selections!) (position!))
    :nothing (doto ctx (clean!) (nothing!) (selections!) (position!))
    (doto ctx (total!) (selections!) (position!))))