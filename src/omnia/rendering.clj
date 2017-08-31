(ns omnia.rendering
  (require [omnia.input :as i]
           [lanterna.terminal :as t]
           [clojure.core.match :as m]
           [omnia.highlight-beta :as h]
           [omnia.config :as c]
           [omnia.more :refer [map-vals reduce-idx zip-all]]))

(declare total! diff! nothing!)

(defn- no-cs [cs]
  (let [text-colour (cs h/-text :white)]
    (map-vals (constantly text-colour) cs)))

(defn- clean-cs [cs]
  (assoc cs h/-select :default))

(defn- select-cs [cs]
  (-> (no-cs cs)
      (assoc h/-select (cs h/-select))))

(defn- pad-erase [current-line former-line]
  (let [hc      (count current-line)
        hf      (count former-line)
        largest (max hc hf)]
    (->> (repeat \space)
         (take (- largest hc))
         (concat current-line)
         (vec))))

(defn- display! [emission terminal x y]
  (reduce-idx (fn [ix _ input] (t/put-character terminal input ix y)) x nil emission))

(defn screen-y
  "gy = cy - (h - fov - ov)
   cy = gy + (h - fov - ov)"
  ([hud]
   (screen-y hud (-> hud :cursor second)))
  ([hud y']
   (let [{fov :fov
          ov  :ov
          h   :height} hud]
     (if (> h fov)
       (- y' (- h fov ov))
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
         scroll? :scroll?} hud
        take-right (fn [n coll] (lazy-seq (take-last n coll)))]
    (if scroll?
      (i/rebase hud #(->> % (take-right lor) (take fov)))
      (i/rebase hud #(->> % (drop-last ov) (take-right fov))))))

(defn- when-unpaged [ctx f]
  (let [{terminal :terminal
         complete :complete-hud
         previous :previous-hud} ctx
        current (project-hud complete)
        former  (project-hud previous)]
    (if (not= (:ov current) (:ov former))
      (total! ctx)
      (f terminal current former))))

(defn region [hud selection]
  (let [{[xs ys] :start
         [xe ye] :end} selection]
    (-> (i/deselect hud)
        (i/reset-y ys)
        (i/reset-x xs)
        (i/select)
        (i/reset-y ye)
        (i/reset-x xe)
        (i/extract)
        (:lines))))

(defn print-line! [line terminal cs [x y]]
  (let [ix (atom x)]
    (h/process line
               (fn [emission type]
                 (let [colour (cs type)]
                   (t/set-fg-color terminal colour)
                   (display! emission terminal @ix y)
                   (swap! ix #(+ % (count emission))))))))

(defn print! [hud terminal cs]
  (reduce-idx (fn [y _ line] (print-line! line terminal cs [0 y])) nil (:lines hud)))

(defn highlight! [ctx regions]
  (let [{terminal :terminal
         complete :complete-hud
         cs       :colourscheme} ctx
        fov (:fov complete)
        bg-colour (cs h/-select)]
    (run!
      #(let [projection (project-selection % fov)
             {[xs ys] :start
              [xe ye] :end} projection]
         (->> (region complete projection)
              (reduce-idx
                (fn [y x line]
                  (t/set-bg-color terminal bg-colour)
                  (print-line! line terminal cs [x (screen-y complete y)])
                  0) ys xs))) regions)
    (t/set-bg-color terminal :default)))

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

;; === Rendering strategies ===

(defn total! [ctx]
  (let [{terminal  :terminal
         complete  :complete-hud
         cs        :colourscheme} ctx]
    (t/clear terminal)
    (print! (project-hud complete) terminal cs)))

(defn diff! [{:keys [colourscheme] :as ctx}]
  (when-unpaged
    ctx
    (fn [terminal current former]
      (->> (:lines former)
           (zip-all (:lines current))
           (map-indexed (fn [idx paired] (conj paired idx)))
           (drop-while (fn [[current-line former-line _]] (= current-line former-line)))
           (map (fn [[current-line former-line y]] [(pad-erase current-line former-line) y]))
           (run! (fn [[line y]] (print-line! line terminal colourscheme [0 y])))))))

(defn nothing! [ctx]
  (when-unpaged ctx (fn [_ _ _] ())))

(defn render [ctx]
  (let [{terminal :terminal
         complete :complete-hud} ctx
        [x y] (project-cursor complete)]
    (case (:render ctx)
      :diff (doto ctx (clean!) (diff!) (selections!))
      :nothing (doto ctx (clean!) (nothing!) (selections!))
      (doto ctx (total!) (selections!)))
    (t/move-cursor terminal x y)))