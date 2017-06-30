(ns omnia.rendering
  (require [omnia.input :as i]
           [lanterna.terminal :as t]
           [clojure.core.match :as m]
           [omnia.highlight :refer [slc-bg s0 process]]
           [omnia.more :refer [take-right reduce-idx zip-all map-vals]]))

(declare total! diff! nothing!)

(defn- selection-scheme [colourscheme]
  (-> (fn [_] :white)
      (map-vals colourscheme)
      (assoc slc-bg (colourscheme slc-bg))))

(defn- clean-up-scheme [colourscheme]
  (assoc colourscheme slc-bg :default))

(defn- pad-erase [current-line former-line]
  (let [hc      (count current-line)
        hf      (count former-line)
        largest (max hc hf)]
    (->> (repeat \space)
         (take (- largest hc))
         (concat current-line)
         (vec))))

(defn screen-y
  "gy = cy - (h - fov - ov)
   cy = gy + (h - fov - ov)"
  ([hud]
   (screen-y hud (-> hud :cursor second)))
  ([hud y']
   (let [{fov :fov
          ov  :ov
          h   :height} hud]
     (if (> @h fov)
       (- y' (- @h fov ov))
       y'))))

(defn project-cursor [hud]
  (let [[x hy] (:cursor hud)
        y (screen-y hud hy)]
    [x y]))

(defn project-selection [selection fov]
  (let [{[xs ys] :start
         [xe ye] :end} selection
        start-y (- ye fov)
        start (if (> start-y ys) [0 (inc start-y)] [xs ys])]
    {:start start
     :end [xe ye]}))

(defn project-hud [hud]
  (let [{lor     :lor
         fov     :fov
         ov      :ov
         scroll? :scroll?} hud]
    (if scroll?
      (i/rebase hud #(->> % (take-right lor) (take fov)))
      (i/rebase hud #(->> % (drop-last ov) (take-right fov))))))

(defn- when-unscrolled [ctx f]
  (let [{terminal :terminal
         complete :complete-hud
         previous :previous-hud} ctx
        current (project-hud complete)
        former  (project-hud previous)]
    (if (not= (:ov current) (:ov former))
      (total! ctx)
      (f terminal current former))))

(defn highlight! [ctx regions]
  (let [{terminal     :terminal
         complete     :complete-hud
         colourscheme :colourscheme} ctx
        fov (:fov complete)
        bg-colour (get colourscheme slc-bg)]
    (->> regions
         (mapv #(project-selection % fov))
         (run!
           (fn [{[xs ys] :start
                 [xe ye] :end}]
             (let [start-state (-> complete
                                   (i/reset-y ys)
                                   (i/reset-x (dec xs))
                                   (i/center)
                                   (process s0 colourscheme)
                                   (first))]
               (loop [x xs
                      y ys
                      state start-state]
                 (let [sy (screen-y complete y)
                       ch (i/sym-at complete [x y])
                       [next-state colour] (process ch state colourscheme)]
                   (cond
                     (> y ye) ()
                     (and (>= y ye) (>= x xe)) ()
                     (nil? ch) (recur 0 (inc y) s0)
                     :else (do
                             (doto terminal
                               (t/set-bg-color bg-colour)
                               (t/set-fg-color colour)
                               (t/put-character ch x sy))
                             (recur (inc x) y next-state)))))))))
    (t/set-bg-color terminal :default)))

(defn selections! [ctx]
  (-> ctx
      (update :colourscheme selection-scheme)
      (highlight! (:highlights ctx))))

(defn clean! [ctx]
  ;; Always re-render from the beginning of the line to avoid syntax highlighting artifacts
  (letfn [(reset [selection]
            (update selection :start (fn [[_ y]] [0 y])))
          (clean-up! [context] (highlight! context (:garbage context)))]
    (-> ctx
        (update :colourscheme clean-up-scheme)
        (update :garbage #(mapv reset %))
        (clean-up!))))

(defn print-row! [y terminal line colourscheme]
  (reduce-idx
    (fn [x state input]
      (let [[next-state colour] (process input state colourscheme)]
        (doto terminal
          (t/set-fg-color colour)
          (t/put-character input x y))
        next-state)) s0 line))

(defn print! [terminal hud colourscheme]
  (reduce-idx
    (fn [y _ line] (print-row! y terminal line colourscheme))
    nil (:lines hud)))

;; === Rendering strategies ===

(defn total! [ctx]
  (let [{terminal     :terminal
         complete     :complete-hud
         colourscheme :colourscheme} ctx]
    (doto terminal
      (t/clear)
      (print! (project-hud complete) colourscheme))))

(defn diff! [ctx]
  (let [cs (:colourscheme ctx)]
    (when-unscrolled ctx
                     (fn [terminal current former]
                       (->> (:lines former)
                            (zip-all (:lines current))
                            (map-indexed (fn [idx paired] (conj paired idx)))
                            (drop-while (fn [[current-line former-line _]] (= current-line former-line)))
                            (map (fn [[current-line former-line y]] [(pad-erase current-line former-line) y]))
                            (run! (fn [[line y]] (print-row! y terminal line cs))))))))

(defn nothing! [ctx]
  (when-unscrolled ctx (fn [_ _ _] ())))

(defn render [ctx]
  (let [{terminal :terminal
         complete :complete-hud} ctx
        [x y] (project-cursor complete)]
    (case (:render ctx)
      :diff (doto ctx (clean!) (diff!) (selections!))
      :nothing (doto ctx (clean!) (nothing!) (selections!))
      (doto ctx (total!) (selections!)))
    (t/move-cursor terminal x y)))
