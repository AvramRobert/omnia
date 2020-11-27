(ns omnia.hud
  (:require [schema.core :as s]
            [omnia.input :as i]
            [omnia.more :refer [Point Region omnia-version ++ -- mod* inc< dec<]]
            [omnia.input :refer [Seeker]]
            [omnia.terminal :refer [Terminal]]
            [omnia.server :refer [REPLServer]]))

(def caret (i/from-string "Ω =>"))
(def continuation (i/from-string "..."))
(def delimiter (i/from-string "------"))

(def Hud
  {:seeker  Seeker
   :lor     s/Int
   :fov     s/Int
   :ov      s/Int
   :scroll? s/Bool})

(s/defn hud :- Hud
  "A hud is a structure enclosing some form of text that supports
   projecting that text within a bounded view.
   It uses the following attributes to keep track of the projection.
   fov = field of view
       -> the size of the projected view. Amount of lines that can be viewed at one time.

   lor = line of reference
       -> the offset of scrolling. How many lines left when scrolling upwards.

   ov  = overview
       -> offset withing the bounded view.
       -> Even though the view is bounded by `fov`, the cursor can still navigate the entire text.
          To move the view properly, this offset keeps track of when the cursor has moved beyond the bounds
          of `fov` and by how many lines

   scroll? = scrolling flag
       -> indicates if it's currently being scrolled"
  [seeker :- Seeker
   fov    :- s/Int]
  {:seeker seeker
   :fov    fov
   :lor    fov
   :ov     0
   :scroll? false})

(s/defn hud-of :- Hud
  [fov :- s/Int]
  (hud i/empty-seeker fov))

(s/def empty-hud :- Hud
  (hud-of 0))

(s/defn bottom-y :- s/Int
  [hud :- Hud]
  "The lower y bound of a page (exclusive)
  bottom-y = height - ov - 1
  Subtract 1 because we count from 0"
  (let [ov     (:ov hud)
        height (-> hud (:seeker) (:height))]
    (-- height ov 1)))

(s/defn top-y :- s/Int
  [hud :- Hud]
  "The upper y bound of a page (inclusive)
  top-y = (height - fov - ov)"
  (let [fov    (:fov hud)
        ov     (:ov hud)
        height (-> hud (:seeker) (:height))]
    (-- height fov ov)))

(s/defn project-y :- s/Int
  [hud :- Hud, y :- s/Int]
  "given hud-y, screen-y = hud-y - top-y
   given screen-y, hud-y = screen-y + top-y"
  (let [fov (:fov hud)
        h   (-> hud (:seeker) (:height))
        ys  (top-y hud)]
    (if (> h fov) (- y ys) y)))

(s/defn project-cursor :- Point
  [hud :- Hud]
  (let [[x hy] (-> hud :seeker :cursor)
        y (project-y hud hy)]
    [x y]))

(s/defn project-selection :- Region
  [hud :- Hud
   selection :- Region]
  (let [fov             (:fov hud)
        h               (-> hud (:seeker) (:height))
        [xs ys] (:start selection)
        [xe ye] (:end selection)
        top             (top-y hud)
        bottom          (bottom-y hud)
        unpaged?        (< h fov)
        clipped-top?    (< ys top)
        clipped-bottom? (> ye bottom)
        visible-top?    (<= top ys bottom)
        visible-bottom? (<= top ye bottom)
        end-bottom      (-> hud (:seeker) (i/reset-y bottom) (i/end-x) (:cursor))]
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

(s/defn project-hud :- Seeker
  [hud :- Hud]
  (let [{seeker  :seeker
         lor     :lor
         fov     :fov
         ov      :ov
         scroll? :scroll?} hud
        take-right (fn [n coll]
                     (lazy-seq (take-last n coll)))]
    (if scroll?
      (i/rebase seeker #(->> % (take-right lor) (take fov)))
      (i/rebase seeker #(->> % (drop-last ov) (take-right fov))))))

(s/defn correct-ov :- s/Int
  [hud :- Hud
   previous-hud :- Hud]
  (let [{fov                       :fov
         ov                        :ov
         {h :height [_ y] :cursor} :seeker} hud
        {pfov         :fov
         {ph :height} :seeker} previous-hud
        upper-y     (top-y hud)                             ;; the top viewable y
        lower-y     (bottom-y hud)                          ;; the lower viewable y
        over-upper? (< y upper-y)
        over-lower? (> y lower-y)
        at-lower?   (= y lower-y)
        smaller?    (< h ph)
        larger?     (> h ph)
        unpaged?    (and (<= h fov)
                         (<= ph fov))
        resized?    (and (not= pfov fov)
                         (not= 0 ov))]
    (cond
      resized? (++ ov (- pfov fov))                         ;; we've changed the terminal size
      unpaged? ov                                           ;; we've not exceeded the fov
      (and larger? at-lower?) ov                            ;; we've gotten bigger but we're still at the bottom
      (or larger? smaller?) (++ ov (- h ph))                ;; we've changed in size
      over-upper? (++ ov (- upper-y y))                     ;; we've exceeded the upper bound
      over-lower? (-- ov (- y lower-y))                     ;; we've exceeded the lower bound
      :else ov)))

(s/defn corrected :- Hud
  ([hud :- Hud]
   (corrected hud hud))
  ([hud :- Hud,
    previous-hud :- Hud]
   (assoc hud :ov (correct-ov hud previous-hud))))

(s/defn paginate :- Seeker
  [hud :- Hud]
  (let [cursor     (project-cursor hud)
        truncated? (-> hud (:ov) (zero?) (not))
        extend     #(if truncated? (i/adjoin % continuation) %)]
    (-> hud
        (update :seeker #(i/reset-to % cursor))
        (project-hud)
        (extend)
        (i/indent 1))))

(s/defn enrich-with :- Hud
  [hud :- Hud, seekers :- [Seeker]]
  (update hud :seeker #(apply i/conjoin % seekers)))

(s/defn riffle-window :- Hud
  [seeker :- Seeker
   size :- s/Int]
  (let [content (->> seeker (i/start) (i/end-x))]
    (-> (hud-of size) (enrich-with [content]) (corrected))))

(s/defn riffle [hud :- Hud] :- Hud
  (let [[_ y] (-> hud :seeker :cursor)
        height (-> hud :seeker :height)
        y'     (mod* (inc y) height)]
    (-> hud (update :seeker #(i/reset-y % y')) (corrected))))

(s/defn nowards :- s/Int
  [hud :- Hud]
  (let [fov    (:fov hud)
        ov     (:ov hud)
        height (-> hud :seeker :height)]
    (if (> height fov) (+ fov ov) height)))

(s/defn upwards :- s/Int
  [hud :- Hud]
  (let [lor    (:lor hud)
        height (-> hud :seeker :height)]
    (inc< lor height)))

(s/defn downwards :- s/Int
  [hud :- Hud]
  (let [lor (:lor hud)]
    (dec< lor (nowards hud))))

(s/defn scroll :- Hud
  [hud :- Hud f]
  (-> hud
      (assoc :scroll? true)
      (assoc :lor (f hud))))

(s/defn scroll-stop :- Hud
  [hud :- Hud]
  (-> hud
      (scroll nowards)
      (assoc :scroll? false)))

(s/defn scroll-up :- Hud
  [hud :- Hud]
  (scroll hud upwards))

(s/defn scroll-down :- Hud
  [hud :- Hud]
  (scroll hud downwards))

(s/defn hollow? :- s/Bool
  [hud :- Hud]
  (-> hud :seeker :lines empty?))

(s/defn view :- String
  [hud :- Hud]
  (-> hud (:seeker) (i/debug-string)))

(s/defn text :- Seeker
  [hud :- Hud]
  (:seeker hud))

(s/defn current-line :- [Character]
  [hud :- Hud]
  (-> hud (text) (i/line)))

(s/defn deselect :- Hud
  [hud :- Hud]
  (update hud :seeker i/deselect))

(s/defn reset-overview :- Hud
  [hud :- Hud]
  (assoc hud :ov 0))

(s/defn overview :- s/Int
  [hud :- Hud]
  (:ov hud))

(s/defn field-of-view :- s/Int
  [hud :- Hud]
  (:fov hud))

(s/defn reset-view :- Hud
  [hud :- Hud, size :- s/Int]
  (assoc hud :fov size :lor size))

(s/defn pop-up :- Hud
  [hud :- Hud, embedded :- Hud]
  (let [text      (:seeker hud)
        paginated (paginate embedded)
        ph        (:height text)
        top       (-> text (i/peer (fn [l [x & _]] (conj l x))))
        bottom    (-> text (i/peer (fn [_ [_ & r]] (drop (+ ph 2) r))))]
    (assoc hud :seeker (-> (i/conjoin top delimiter paginated)
                           (i/end-x)
                           (i/adjoin delimiter bottom)))))