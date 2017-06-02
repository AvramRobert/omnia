(ns omnia.rendering
  (use omnia.highlight
       omnia.more)
  (require [omnia.input :as i]
           [lanterna.terminal :as t]
           [clojure.core.match :as m]))

(declare total! diff! input! nothing!)

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

(defn project-selection
  ([hud]
    (project-selection hud (i/selection hud)))
  ([hud selection]
   (let [fov (:fov hud)
         {[xs ys] :start
          [xe ye] :end} selection
         start-y (- ye fov)
         start (if (> start-y ys) [0 (inc start-y)] [xs ys])]
     {:start start
      :end [xe ye]})))

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


(defn hightlight-selection! [terminal hud selection colour]
  (let [{[xs ys] :start
         [xe ye] :end} selection]
    (loop [x xs
           y ys]
      (let [ch (i/sym-at hud [x y])
            sy (screen-y hud y)]
        (cond
          (> y ye) ()
          (and (>= y ye) (>= x xe)) ()
          (not (nil? ch)) (do
                            (doto terminal
                              (t/set-bg-color colour)
                              (t/put-character ch x sy))
                            (recur (inc x) y))
          :else (recur 0 (inc y)))))
    (t/set-bg-color terminal :default)))


(defn highlight!
  ([ctx regions]
    (highlight! ctx regions :blue))
  ([ctx regions colour]
    (let [{terminal :terminal
           complete :complete-hud} ctx]
      (foreach
        #(hightlight-selection! terminal complete (project-selection complete %) colour) regions))))

(defn selections! [ctx]
  (highlight! ctx (:highlights ctx)))

(defn clean! [ctx]
  (highlight! ctx (:garbage ctx) :default))

(defn print-row! [y terminal line]
  (reduce-idx
    (fn [x state c]
      (let [[next-state colour] (process state c)]
        (doto terminal
          (t/set-fg-color colour)
          (t/put-character c x y))
        next-state)) s0 line))

(defn print! [terminal seeker]
  (reduce-idx
    (fn [y _ line] (print-row! y terminal line))
    nil (:lines seeker)))

;; === Rendering strategies ===

(defn total! [ctx]
  (let [{terminal :terminal
         complete :complete-hud} ctx]
    (doto terminal
      (t/clear)
      (print! (project-hud complete)))))

(defn diff! [ctx]
  (when-unscrolled ctx
                   (fn [terminal current former]
                     (->> (:lines former)
                          (zip-all (:lines current))
                          (map-indexed (fn [idx paired] (conj paired idx)))
                          (drop-while (fn [[current-line former-line _]] (= current-line former-line)))
                          (map (fn [[current-line former-line y]] [(pad-erase current-line former-line) y]))
                          (foreach (fn [[line y]] (print-row! y terminal line)))))))

(defn input! [ctx]
  (let [{persisted :persisted-hud
         seeker    :seeker} ctx
        padding (->> i/empty-vec (repeat) (take (i/height seeker)) (vec) (i/seeker))]
    (-> ctx
        (assoc :previous-hud (i/join persisted padding))
        (diff!))))

(defn nothing! [ctx]
  (when-unscrolled ctx (fn [_ _ _] ())))

(defn render-context [ctx]
  (let [{terminal :terminal
         complete :complete-hud} ctx
        [x y] (project-cursor complete)]
    (case (:render ctx)
      :diff (doto ctx (clean!) (diff!) (selections!))
      :input (doto ctx (clean!) (input!) (selections!))
      :nothing (doto ctx (clean!) (nothing!) (selections!))
      (doto ctx (total!) (selections!)))
    (t/move-cursor terminal x y)))
