(ns omnia.rendering
  (require [omnia.terminal :as t]
           [omnia.input :as i]
           [omnia.highlight :as h]
           [omnia.more :refer [map-vals reduce-idx zip-all --]]))

(declare total! diff! nothing!)

;; === Various colourschemes ===

(defn no-cs [cs]
  (let [text-colour (cs h/-text :white)]
    (map-vals (constantly text-colour) cs)))

(defn clean-cs [cs]
  (assoc cs h/-select :default h/-back :default))

(defn select-cs [cs]
  (-> (no-cs cs)
      (assoc h/-back (cs h/-select)
             h/-select (cs h/-select))))

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
    (-> (i/deselect hud)
        (i/reset-y ys)
        (i/reset-x xs)
        (i/select)
        (i/reset-y ye)
        (i/reset-x xe)
        (i/extract)
        (assoc :height (:height hud)))))                    ;; for accurate projection

(defn- display! [emission terminal x y]
  (reduce-idx (fn [ix _ input] (t/put! terminal input ix y)) x nil emission))

(defn print-line! [line terminal cs [x y]]
  (let [ix (atom x)]
    (t/background! terminal (cs h/-back))
    (h/process line
               (fn [emission type]
                 (t/foreground! terminal (cs type))
                 (display! emission terminal @ix y)
                 (swap! ix #(+ % (count emission)))))
    (t/background! terminal :default)))

(defn print-hud!
  ([hud terminal cs]
   (print-hud! hud terminal cs [0 0]))
  ([hud terminal cs [x y]]
   (reduce-idx
     (fn [cy cx line]
       (print-line! line terminal cs [cx (project-y hud cy)])
       0) y x (:lines hud))))

(defn highlight! [ctx regions]
  (let [{terminal :terminal
         complete :complete-hud
         cs       :colourscheme} ctx
        fov (:fov complete)]
    (run!
      #(let [projection (project-selection complete %)
             {[xs ys] :start
              [xe ye] :end} projection]
         (-> (region complete projection)
             (print-hud! terminal cs [xs ys]))) regions)))

(defn clean! [ctx]
  ;; Always re-render from the beginning of the line to avoid syntax highlighting artifacts
  (letfn [(reset [selection]
            (update selection :start (fn [[_ y]] [0 y])))]
    (as-> ctx context
          (update context :colourscheme clean-cs)
          (update context :garbage #(mapv reset %))
          (highlight! context (:garbage context)))))

(defn selections! [ctx]
  (-> ctx
      (update :colourscheme select-cs)
      (highlight! (:highlights ctx))))

(defn position! [{:keys [terminal complete-hud]}]
  (let [[x y] (project-cursor complete-hud)]
    (t/move! terminal x y)))

;; === Rendering strategies ===

(defn- pad-erase [current-line former-line]
  (let [hc      (count current-line)
        hf      (count former-line)
        largest (max hc hf)]
    (->> (repeat \space)
         (take (- largest hc))
         (concat current-line)
         (vec))))

(defn- when-unpaged [ctx f]
  (let [{terminal :terminal
         complete :complete-hud
         previous :previous-hud} ctx
        current (project-hud complete)
        former  (project-hud previous)]
    (if (not= (:ov current) (:ov former))
      (total! ctx)
      (f terminal current former))))

(defn total! [ctx]
  (let [{terminal :terminal
         complete :complete-hud
         cs       :colourscheme} ctx]
    (t/clear! terminal)
    (print-hud! (project-hud complete) terminal cs)))

(defn diff! [{:keys [colourscheme] :as ctx}]
  (when-unpaged
    ctx
    (fn [terminal current former]
      (->> (zip-all (:lines current) (:lines former))
           (map-indexed (fn [idx paired] (conj paired idx)))
           (drop-while (fn [[current-line former-line _]] (= current-line former-line)))
           (map (fn [[current-line former-line y]] [(pad-erase current-line former-line) y]))
           (run! (fn [[line y]] (print-line! line terminal colourscheme [0 y])))))))

(defn nothing! [ctx]
  (when-unpaged ctx (fn [_ _ _] ())))

(defn render [ctx]
  (case (:render ctx)
    :diff (doto ctx (clean!) (diff!) (selections!) (position!))
    :nothing (doto ctx (clean!) (nothing!) (selections!) (position!))
    (doto ctx (total!) (selections!) (position!))))